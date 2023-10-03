package rockets

import params._
import params.config._
import rockets.PrivEnum._
import rockets.utils.PLRU
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
  val io = TLBIO()

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
    val hits = Vec(
      (0 until depth).map(i => validBits(i) && tags(i) === tag)
    ).asBits
    val hit = hits.orR

    /** replace policy */
    val replace = PLRU(depth)
    val (nextEntryValid, nextEntry) = validBits.asBools.sFindFirst(!_)
    val replaceEntry = nextEntryValid ? nextEntry | replace.get
  }

  /** entry store in tag_ram, include ppn, valid and permissions */
  case class tagRamEntry() extends Bundle {
    val ppn: UInt = UInt(ppnBits bits)
    val valid, dirty, ur, uw, ux, sr, sw, sx = Bool()
  }

  /** ppn store buffer */
  val tagRam = new Mem(tagRamEntry(), depth).setName("tag_ram")

  /** privilege for current request */
  val priv = Mux(
    io.ptw.status.mprv & !io.req.payload.instr,
    io.ptw.status.prv1,
    io.ptw.status.prv
  )
  val privS = priv === PRV_S
  val privUseVm = priv.asBits.asUInt <= PRV_S.asBits.asUInt
  val vmEnabled = io.ptw.status.vm(3) & privUseVm

  /** firstly, read tag_ram and tag_cam */
  tagCam.tag := io.req.payload.asid @@ io.req.payload.vpn
  val entry = tagRam.readAsync(OHToUInt(tagCam.hits))
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
    io.req.payload.vpn(0, ppnBits bits)
  )

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

    sRequest.whenIsActive {}
  }

  /** send ptw request */
  val reqReg = RegInit {
    val r = TLBReq()
    r.assignFromBits(B(0))
    r
  }

  /** to update tag cam */
  val refillTagReg = RegInit(U(0, widthOf(tagCam.tag) bits))
  val refillAddrReg = RegInit(U(0, widthOf(tagCam.writeAddr) bits))

  /** idle -> request */
  when(fsm.isExiting(fsm.sIdle)) {
    reqReg := io.req.payload
    refillTagReg := io.req.payload.asid @@ io.req.payload.vpn
    refillAddrReg := tagCam.replaceEntry
  }
}
