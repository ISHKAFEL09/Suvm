package rockets.core.mmu

import rockets.core.enums._
import rockets.generate
import rockets.params._
import rockets.params.config.Parameters
import rockets.tilelink._
import rockets.utils._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

trait HasTLBParameters extends HasDCacheParams {
  val depth: Int = tlbDepth
}

abstract class TLBBundle(implicit val p: Parameters)
    extends Bundle
    with HasTLBParameters

abstract class TLBComponent(implicit val p: Parameters)
    extends Component
    with HasTLBParameters

case class TLBReq()(implicit p: Parameters) extends TLBBundle {
  val asid: UInt = UInt(asIdBits bits)

  /** refer to riscv privilege spec
    */
  val vpn: UInt = UInt(vpnBits + 1 bits)

  val bypass, instr, store = Bool()

  override def clone(): Bundle = TLBReq()
}

case class TLBResp()(implicit p: Parameters) extends TLBBundle {
  val miss, xcptLD, xcptST, xcptIF = Bool()

  /** refer to riscv privilege spec
    */
  val ppn: UInt = UInt(ppnBits bits)

  /** one hot hit index
    */
  val hitIdx: UInt = UInt(depth bits)
}

case class TLBIO()(implicit p: Parameters) extends TLBBundle with IMasterSlave {

  /** request for ppn from cpu
    */
  val req = Stream(TLBReq())

  /** response of ppn from buffer
    */
  val resp = Flow(TLBResp())

  /** when miss, request ptw to return the ppn
    */
  val ptw = TLB2PTWIO()

  override def asMaster(): Unit = {
    master(ptw, resp)
    slave(req)
  }
}

case class TLB()(implicit p: Parameters) extends TLBComponent {
  val io = master(TLBIO())

  /** tag CAM memory, store vpn as tag, entry num set by depth
    */
  val tagCam = new Area {

    /** tag cam memory, asid + vpn bits width
      */
    val tags = new Mem(UInt(asIdBits + vpnBits bits), depth).setName("tag_cam")

    /** whether the entry is valid or not */
    val validBits = RegInit(U(0, depth bits))

    /** after response from ptw, update tag in buffer */
    val write = Bool()
    val writeAddr = UInt(log2Up(depth) bits)
    val writeTag = UInt(asIdBits + vpnBits bits)
    when(write) {
      validBits(writeAddr) := True
      tags.write(writeAddr, writeTag)
    }

    /** if ptw require invalidate, clear all entries */
    val clear = Bool()
    val clearMask = UInt(depth bits)
    when(clear) {
      validBits := validBits & ~clearMask
    }

    /** whether correspond tag is store in buffer */
    val tag = UInt(asIdBits + vpnBits bits)
    val hits = Cat(
      (0 until depth).map(i =>
        validBits(i) && (tags.readAsync(U(i, log2Up(depth) bits)) === tag)
      )
    ).asUInt
    val hit = hits.orR

    /** replace policy */
    val replace = PLRU(depth)
    val (nextEntryValid, nextEntry) = validBits.asBools.sFindFirst(!_)
    val replaceEntry = nextEntryValid ? nextEntry | replace.get
  }

  /** entry store in tag_ram, include ppn, valid and permissions */
  case class tagRamEntry() extends Bundle {
    val ppn: UInt = UInt(ppnBits bits)
    val dirty, ur, uw, ux, sr, sw, sx = Bool()
  }

  /** ppn store buffer */
  val tagRam = new Mem(tagRamEntry(), depth).setName("tag_ram")
  val tagValids = RegInit(U(0, depth bits))

  /** privilege for current request */
  val priv = Mux(
    io.ptw.status.mprv & !io.req.payload.instr,
    io.ptw.status.prv1,
    io.ptw.status.prv
  )
  val privS = priv === PrivEnum.PRV_S
  val privUseVm = priv.asBits.asUInt <= PrivEnum.PRV_S.asBits.asUInt
  val vmEnabled = io.ptw.status.vm(3) & privUseVm

  /** firstly, read tag_ram and tag_cam */
  tagCam.tag := io.req.payload.asid @@ io.req.payload.vpn(0, vpnBits bits)
  val hitEntry = OHToUInt(tagCam.hits)
  val entry = tagRam.readAsync(hitEntry)
  val readp = Mux(privS, entry.sr, entry.ur)
  val writep = Mux(privS, entry.sw, entry.uw)
  val execp = Mux(privS, entry.sx, entry.ux)

  /** if hit, response */
  val xcptVpn = io.req.payload.vpn.msb ^ io.req.payload.vpn.rotateLeft(1).msb
  val tagHit = tagCam.hit && (entry.dirty | !(writep && io.req.payload.store))
  val tlbHit = vmEnabled && tagHit
  val tlbMiss = vmEnabled && !tagHit && !xcptVpn
  io.resp.valid := io.req.fire
  io.resp.payload.hitIdx := tagCam.hits
  io.resp.payload.ppn := Mux(
    vmEnabled & !io.req.payload.bypass,
    entry.ppn,
    io.req.payload.vpn.resized
  )
  io.resp.miss := tlbMiss

  /** update plru */
  when(io.req.fire && tlbHit) {
    tagCam.replace.set(hitEntry)
  }

  /** exception */
  io.resp.payload.xcptLD := xcptVpn | (tlbHit & !readp)
  io.resp.payload.xcptST := xcptVpn | (tlbHit & !writep)
  io.resp.payload.xcptIF := xcptVpn | (tlbHit & !execp)

  /** if miss, send req to ptw */
  /** fsm */
  val fsm = new StateMachine {
    val sIdle = StateEntryPoint()
    val sRequest, sWaitInvalidate, sWait = State()

    sIdle.whenIsActive {
      when(io.req.fire && tlbMiss) { goto(sRequest) }
    }

    sRequest.whenIsActive {
      when(io.ptw.invalidate) {
        goto(sIdle)
        when(io.ptw.req.ready) {
          goto(sWaitInvalidate)
        }
      } elsewhen (io.ptw.req.ready) {
        goto(sWait)
      }
    }

    sWaitInvalidate.whenIsActive {
      when(io.ptw.resp.fire) {
        goto(sIdle)
      }
    }

    sWait.whenIsActive {
      when(io.ptw.invalidate) {
        goto(sWaitInvalidate)
      } elsewhen (io.ptw.resp.fire) {
        goto(sIdle)
      }
    }
  }

  /** ready to receive new request */
  io.req.ready := fsm.isActive(fsm.sIdle)

  /** to send ptw request */
  val reqReg = RegInit {
    val r = TLBReq()
    r.assignFromBits(B(0, widthOf(r) bits))
    r
  }

  /** to update tag cam */
  val refillTagReg = RegInit(U(0, widthOf(tagCam.tag) bits))
  val refillAddrReg = RegInit(U(0, widthOf(tagCam.writeAddr) bits))

  /** idle -> request */
  when(fsm.isExiting(fsm.sIdle)) {
    reqReg := io.req.payload
    refillTagReg := io.req.payload.asid @@ io.req.payload.vpn(0, vpnBits bits)
    refillAddrReg := tagCam.replaceEntry
  }

  /** send ptw request */
  io.ptw.req.valid := fsm.isActive(fsm.sRequest)
  io.ptw.req.payload.addr := refillTagReg.resized
  io.ptw.req.payload.store := reqReg.store
  io.ptw.req.payload.prv := io.ptw.status.prv
  io.ptw.req.payload.fetch := reqReg.instr

  /** update cam & ram */
  val pte = io.ptw.resp.pte
  val refillEntry = tagRamEntry()
  refillEntry.ppn := pte.ppn
  refillEntry.ur := pte.ur() && !io.ptw.resp.error
  refillEntry.uw := pte.uw() && !io.ptw.resp.error
  refillEntry.ux := pte.ux() && !io.ptw.resp.error
  refillEntry.sr := pte.sr() && !io.ptw.resp.error
  refillEntry.sw := pte.sw() && !io.ptw.resp.error
  refillEntry.sx := pte.sx() && !io.ptw.resp.error
  refillEntry.dirty := pte.dirty
  when(io.ptw.resp.fire) {
    tagRam.write(refillAddrReg, refillEntry)
    tagValids(refillAddrReg) := !io.ptw.resp.error
  }

  tagCam.write := fsm.isExiting(fsm.sWait) && fsm.isEntering(fsm.sIdle)
  tagCam.writeAddr := refillAddrReg
  tagCam.writeTag := refillTagReg

  /** clear all entries on flush, or invalid entries on access
    */
  tagCam.clear := io.ptw.invalidate || io.req.fire
  val invalidEntry =
    ~tagValids | ((tagCam.hit && ~tagHit).asUInt(depth bits) |<< hitEntry)
  tagCam.clearMask := io.ptw.invalidate ? UInt(depth bits)
    .setAll() | invalidEntry
}

object TLBApp extends App {
  implicit def config: Parameters = Parameters((_, _, _) => {
    case TileKey =>
      new TileParams {
        override val core: CoreParams = new CoreParams {
          override val xLen: Int = 32
          override val retireWidth: Int = 0
          override val coreFetchWidth: Int = 0
          override val coreInstBits: Int = 32
          override val coreDCacheRegTagBits: Int = 24
          override val fastLoadByte: Boolean = false
          override val fastLoadWord: Boolean = false
          override val maxHartIdBits: Int = 1
        }
        override val dCache: DCacheParams = new DCacheParams {
          override val nSDQ: Int = 8
          override val nMSHRs: Int = 1
          override val nTLBs: Int = 16
          override val nSets: Int = 8
          override val blockOffBits: Int = 6
          override val nWays: Int = 4
          override val rowBits: Int = 128
          override val code: Option[Code] = None
        }
        override val link: LinkParams = new LinkParams {
          override val pAddrBits: Int = 34
          override val vAddrBits: Int = 32
          override val pgIdxBits: Int = 12
          override val ppnBits: Int = 22
          override val vpnBits: Int = 20
          override val pgLevels: Int = 2
          override val asIdBits: Int = 1
          override val pgLevelBits: Int = 10
          override val TLDataBeats: Int = 4
          override val TLDataBits: Int = 128
          override val coherencePolicy: CoherencePolicy =
            MESICoherence(new DirectoryRepresentation() {
              override val width: Int = 0
            })

          /** unique name per TL network */
          override val TLId: String = "nbdcache"

          /** manager agents number for this network */
          override val TLManagerNum: Int = 1

          /** client agents number for this network */
          override val TLClientNum: Int = 1

          /** number of client agents that cache data */
          override val TLCacheClientNum: Int = 1

          /** number of client agents that do not cache data */
          override val TLNoCacheClientNum: Int = 0

          /** maximum number of outstanding xact per client */
          override val TLMaxClientOst: Int = 1

          /** maximum number of clients multiplexed onto one port */
          override val TLMaxClientsPerPort: Int = 1

          /** maximum number of outstanding xact per manager */
          override val TLMaxManagerOst: Int = 1

          /** width of cache block address */
          override val TLBlockAddrBits: Int = 7

          /** amo alu op size */
          override val AmoOperandBits: Int = 32
        }
      }
    case _ =>
  })
  generate(TLB())
}
