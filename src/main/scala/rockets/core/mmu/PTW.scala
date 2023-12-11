package rockets.core.mmu

import rockets.core.cache.nbdcache.NBDIO
import rockets.core.consts.MemOps._
import rockets.core.enums._
import rockets.core.registers.MStatus
import rockets.params.HasDCacheParams
import rockets.params.config.Parameters
import rockets.utils.PLRU
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

abstract class PTWBundle(implicit val p: Parameters)
    extends Bundle
    with HasDCacheParams

abstract class PTWComponent(implicit val p: Parameters)
    extends Component
    with HasDCacheParams

/** pte entry, refer to priv-spec
  */
case class PTE()(implicit p: Parameters) extends PTWBundle {
  val ppn: UInt = UInt(ppnBits bits)
  val dirty, ref, valid = Bool()
  val typ: UInt = UInt(4 bits)

  def table(dummy: Int = 0): Bool = valid && typ < 2

  def leaf(dummy: Int = 0): Bool = valid && typ >= 2

  def ur(dummy: Int = 0): Bool = leaf() && typ < 8

  def uw(dummy: Int = 0): Bool = ur() && typ(0)

  def ux(dummy: Int = 0): Bool = ur() && typ(1)

  def sr(dummy: Int = 0): Bool = leaf()

  def sw(dummy: Int = 0): Bool = leaf() && typ(0)

  def sx(dummy: Int = 0): Bool = valid && typ >= 4 && typ(1)

  def hasAccess(req: PTWReq): Bool = {
    val accS = req.fetch ? sx() | (req.store ? sw() | sr())
    val accU = req.fetch ? ux() | (req.store ? uw() | ur())
    (req.prv === PrivEnum.PRV_S) ? accS | accU
  }

  override def clone(): Bundle = PTE()
}

/** tlb request -> ptw
  */
case class PTWReq()(implicit p: Parameters) extends PTWBundle {

  /** vpn address */
  val addr: UInt = UInt(vpnBits bits)

  /** request privilege */
  val prv: PrivEnum.C = PrivEnum()

  /** request from a store instr, needs write permission */
  val store = Bool()

  /** instruction fetch, needs execute permission */
  val fetch = Bool()

  override def clone(): Bundle = PTWReq()
}

/** ptw response -> tlb
  */
case class PTWResp()(implicit p: Parameters) extends PTWBundle {

  /** correspond pte entry */
  val pte: PTE = PTE()

  /** whether error happened */
  val error: Bool = Bool()
}

/** tlb request to ptw
  */
case class TLB2PTWIO()(implicit p: Parameters)
    extends PTWBundle
    with IMasterSlave {

  /** ptw request */
  val req: Stream[PTWReq] = Stream(PTWReq())

  /** ptw response, no backpress allowed */
  val resp: Flow[PTWResp] = Flow(PTWResp())

  /** csr status from data path */
  val status: MStatus = MStatus()

  /** invalidate all tlb cache */
  val invalidate: Bool = Bool()

  override def asMaster(): Unit = {
    master(req)
    slave(resp)
    in(status, invalidate)
  }
}

/** datapath -> ptw
  */
case class DataPath2PTWIO()(implicit p: Parameters)
    extends PTWBundle
    with IMasterSlave {

  /** ptbr register, refer to base address of page dir */
  val ptbr = UInt(pAddrBits bits)

  /** dpath wants to invalid tlb cache */
  val invalidate = Bool()

  /** mstatus register passed to ptw */
  val status = MStatus()

  override def asMaster(): Unit = {
    out(ptbr, invalidate, status)
  }
}

case class PTW(reqNum: Int)(implicit p: Parameters) extends PTWComponent {
  val io = new Bundle {

    /** n tlb to ptw req */
    val requestor = Vec(slave(TLB2PTWIO()), reqNum)

    /** ptw to dcache req */
    val mem = master(NBDIO())

    /** info from dpath to ptw */
    val dpath = slave(DataPath2PTWIO())
  }

  /** current page level, 1 or 0 */
  val currentLevel = RegInit(U(0, log2Up(pgLevels) bits))

  /** arbitor for inputs */
  val arb = StreamArbiterFactory.roundRobin.build(PTWReq(), reqNum)
  arb.io.inputs <> Vec(io.requestor.map(_.req))
  val reqIdx = RegNextWhen(arb.io.chosen, arb.io.output.fire) init 0

  /** reg for current request */
  val reqReg = RegInit {
    val r = PTWReq()
    r.assignFromBits(B(0, widthOf(r) bits))
    r
  }
  val vpnIdx: UInt =
    reqReg.addr.subdivideIn(pgLevelBits bits).reverse(currentLevel)

  /** reg for valid pte from mem, include pte table and pte leaf */
  val pteReg = RegInit {
    val r = PTE()
    r.assignFromBits(B(0, widthOf(r) bits))
    r
  }

  /** update reqReg & pteReg when input valid */
  when(arb.io.output.fire) {
    reqReg := arb.io.output.payload

    /** ppn first point to ptbr */
    pteReg.ppn := io.dpath.ptbr(pgIdxBits until pAddrBits)
  }

  /** pte read from mem */
  val pte = PTE()
  pte.assignFromBits(io.mem.resp.data.asBits, 0, widthOf(PTE()) bits)

  /** pteReg.ppn + vpnIdx point to pte idx in next level */
  val pteAddr = pteReg.ppn @@ vpnIdx @@ U(0, log2Up(xLen / 8) bits)
  val ppnNextLevel = pteAddr >> pgIdxBits
  val ppnCat = 0 until pgLevels - 1 map { i =>
    val j = pgLevelBits * (pgLevels - i - 1)
    (ppnNextLevel >> j) @@ reqReg.addr(0, j bits)
  }
  val ppnResp =
    (currentLevel === pgLevels - 1) ? ppnNextLevel | ppnCat(currentLevel.resized)

  /** addr -> ppn cache, continuous pte leaf may refer to same pte table */
  val cache = new Area {
    val size = log2Up(pgLevels * 2) // TODO: why?
    val replace = PLRU(size)
    val valid = Vec(RegInit(False), size)
    val tagMem: Mem[UInt] = Mem(UInt(pAddrBits bits), size)
    val dataMem: Mem[UInt] = Mem(UInt(ppnBits bits), size)

    /** hit */
    val hits = 0 until size map { i =>
      tagMem.readAsync(U(i).resized) === pteAddr && valid(i)
    }
    val hit = hits.orR
    val ppn = dataMem.readAsync(OHToUInt(hits))

    /** update cache */
    when(!hit && io.mem.resp.fire && pte.table()) {
      val (notFull, emptyEntry) = valid.sFindFirst(!_)
      val entry = notFull ? emptyEntry | replace.get
      valid(entry) := True
      tagMem(entry) := pteAddr
      dataMem(entry) := pte.ppn
    }
    when(io.dpath.invalidate) {
      valid.foreach(_ := False)
    }
  }
  val useCache = cache.hit && currentLevel < pgLevels - 1

  /** whether need to update pte in mem */
  val needUpdatePTE =
    pte.hasAccess(reqReg) && (!pte.ref || (reqReg.store && !pte.dirty))

  /** control fsm */
  val fsm = new StateMachine {

    /** entry state */
    val sIdle = StateEntryPoint()

    /** request to dcache for pte */
    val sReq = State()

    /** wait pte resp from dcache */
    val sWait = State()

    /** set flag bits of pte in mem when r/d not set */
    val sUpdatePTE = State()

    /** wait set r/d bit done */
    val sWaitUpdate = State()

    /** translate error */
    val sError = State()

    /** ptw done */
    val sDone = State()

    /** sIdle -> sReq */
    sIdle
      .whenIsActive {
        when(arb.io.output.valid) {
          goto(sReq)
        }
      }

    /** sReq -> sWait */
    sReq
      .whenIsActive {
        when(!useCache && io.mem.req.ready) {
          goto(sWait)
        }
      }

    /** sWait -> sReq/sError/sUpdatePTE/sDone */
    sWait
      .whenIsActive {
        when(io.mem.resp.payload.nack) {
          goto(sReq)
        }.elsewhen(io.mem.resp.valid) {
          when(pte.table() && currentLevel < pgLevels - 1) {
            goto(sReq)
          }.elsewhen(pte.leaf()) {
            when(needUpdatePTE) {
              goto(sUpdatePTE)
            }.otherwise {
              goto(sDone)
            }
          }.otherwise {
            goto(sError)
          }
        }
      }

    /** sUpdatePTE -> sWaitUpdate */
    sUpdatePTE
      .whenIsActive {
        when(io.mem.req.fire) { goto(sWaitUpdate) }
      }

    /** sWaitUpdate -> sReq/sUpdatePTE */
    sWaitUpdate
      .whenIsActive {
        when(io.mem.resp.nack) {
          goto(sUpdatePTE)
        }.elsewhen(io.mem.resp.valid) {
          goto(sReq)
        }
      }

    /** sDone -> sReady */
    sDone.whenIsActive(goto(sIdle))

    /** sError -> sReady */
    sError.whenIsActive(goto(sIdle))
  }

  /** logic reference to fsm
    */
  /** arbitor */
  arb.io.output.ready := fsm.isActive(fsm.sIdle)

  /** update pteReg for valid pte, table and leaf */
  when(io.mem.resp.valid && fsm.isActive(fsm.sWait) && !needUpdatePTE) {
    pteReg := pte
  }.elsewhen(fsm.isActive(fsm.sReq) && useCache) {
    pteReg.ppn := cache.ppn
  }

  /** update cache plru */
  when(cache.hit && fsm.isActive(fsm.sReq)) {
    cache.replace.set(OHToUInt(cache.hits))
  }

  /** update level */
  when(fsm.isActive(fsm.sIdle)) { currentLevel := 0 }
    .elsewhen(currentLevel < pgLevels - 1) {
      when(
        (fsm.isActive(fsm.sReq) && cache.hit) ||
          (fsm.isActive(fsm.sWait) && io.mem.resp.fire && pte.table())
      ) {
        currentLevel := currentLevel + 1
      }
    }

  /** send request to mem */
  io.mem.invalidateLR := False
  io.mem.req.valid :=
    (fsm.isActive(fsm.sReq) && !useCache) || fsm.isActive(fsm.sUpdatePTE)
  io.mem.req.payload.phys := True
  io.mem.req.payload.cmd := fsm.isActive(fsm.sReq) ? M_XRD | M_XA_OR
  io.mem.req.payload.typ := MT_D
  io.mem.req.payload.addr := pteAddr
  io.mem.req.payload.kill := False
  io.mem.req.payload.tag := 0
  io.mem.req.payload.data := {
    val pte = PTE()
    pte.allowOverride()
    pte.assignFromBits(B(0, widthOf(pte) bits))
    pte.ref := True
    pte.dirty := reqReg.store
    pte.asBits.asUInt
  }.resized

  /** response to tlb */
  0 until reqNum foreach { i =>
    io.requestor(i).resp.valid :=
      U(i) === reqIdx && (fsm.isActive(fsm.sDone) || fsm.isActive(fsm.sError))
    io.requestor(i).resp.payload.error := fsm.isActive(fsm.sError)
    io.requestor(i).resp.payload.pte := pteReg
    io.requestor(i).resp.payload.pte.allowOverride()
    io.requestor(i).resp.payload.pte.ppn := ppnResp
    io.requestor(i).invalidate := io.dpath.invalidate
    io.requestor(i).status := io.dpath.status
  }
}

object PTWApp extends App {
  import rockets._
  import rockets.tile.Configs._
  generate(PTW(4))
}
