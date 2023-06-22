package rocket2

import chisel3._
import chisel3.util._
import rocket2.PRV._
import rocket2.config._

object TLBEntries extends Field[Int]

trait TLBParameters extends HasCoreParams {
  val entries = p(TLBEntries)
  val camAddrBits = log2Ceil(entries)
  val camTagBits = asIdBits + vpnBits
}

abstract class TLBBundle(implicit val p: Parameters) extends Bundle with TLBParameters
abstract class TLBModule(implicit val p: Parameters) extends Module with TLBParameters

class CamIO(implicit p: Parameters) extends TLBBundle {
  val tag = Input(UInt(camTagBits.W))
  val hit = Output(Bool())
  val validBits = Output(UInt(entries.W))
  val hitBits = Output(UInt(entries.W))
  val write = Input(Bool())
  val writeAddr = Input(UInt(camAddrBits.W))
  val writeTag = Input(UInt(camTagBits.W))
  val clear = Input(Bool())
  val clearBits = Input(UInt(entries.W))
}

class TLBCam(implicit p: Parameters) extends TLBModule {
  val io = IO(new CamIO)

  val tags = Mem(entries, UInt(camTagBits.W))
  val bitmap = RegInit(0.U(entries.W))

  when (io.write) {
    bitmap := bitmap.bitSet(io.writeAddr, true.B)
    tags.write(io.writeAddr, io.writeTag)
  }
  when (io.clear) {
    bitmap := bitmap & (~io.clearBits).asUInt
  }

  io.validBits := bitmap
  io.hitBits := VecInit((0 until entries).map(i => bitmap(i) && tags(i) === io.tag)).asUInt
  io.hit := io.hitBits.orR
}

class PLRU(n: Int) {
  val state = RegInit(0.U(n.W))

  def access(way: UInt): Unit = {
    var nextState = state
    var idx = 1.U(1.W)
    for (i <- log2Up(n) - 1 to 0 by -1) {
      nextState = nextState.bitSet(idx, !way(i))
      idx = Cat(idx, way(i))
    }
    state := nextState
  }

  def replace: UInt = {
    var idx = 1.U(1.W)
    for (_ <- 0 until log2Up(n)) {
      idx = Cat(idx, state(idx.pad(5))) // idx = idx * 2 + {0, 1}
    }
    idx(log2Up(n) - 1, 0)
  }
}

class TLBReq(implicit p: Parameters) extends TLBBundle {
  val asid = UInt(asIdBits.W)
  val vpn = UInt((vpnBits + 1).W)
  val passthrough = Bool()
  val instruction = Bool()
  val store = Bool()
}

class TLBResp(implicit p: Parameters) extends TLBBundle {
  val ppn = Output(UInt(ppnBits.W))
  val miss = Output(Bool())
  val hitIdx = Output(UInt(entries.W))
  val xcptLD = Output(Bool())
  val xcptST = Output(Bool())
  val xcptIF = Output(Bool())
}

class TLBEntry(implicit p: Parameters) extends TLBBundle {
  val valid = Bool()
  val dirty = Bool()
  val uRead = Bool()
  val uWrite = Bool()
  val uExecute = Bool()
  val sRead = Bool()
  val sWrite = Bool()
  val sExecute = Bool()
}

class TLB(implicit p: Parameters) extends TLBModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new TLBReq))
    val resp = new TLBResp
    val ptw = new TLB2PTWIO
  })

  object fsmState extends ChiselEnum {
    val sReady, sRequest, sWait, sWaitInvalidate = Value
  }
  import fsmState._
  val state = RegInit(sReady)
  val nextState = WireDefault(state)
  state := nextState

  // cam & ram
  val plru = new PLRU(entries)
  plru.state.suggestName("plruState")
  val tagCam = Module(new TLBCam)
  val ppnRam = Mem(entries, UInt(ppnBits.W))
  val ppnFlags = RegInit(0.U.asTypeOf(Vec(entries, new TLBEntry)))
  tagCam.io.tag := Cat(io.req.bits.asid, io.req.bits.vpn(vpnBits - 1, 0))

  // privilege & hit
  val priv = Mux(io.ptw.status.mPrv && !io.req.bits.instruction, io.ptw.status.prv1, io.ptw.status.prv)
  val privS = priv === PRV_S.asUInt
  val privUseVm = priv <= PRV_S.asUInt
  val vmEnable = io.ptw.status.vm(3) && privUseVm
  val badVA = io.req.bits.vpn(vpnBits) ^ io.req.bits.vpn(vpnBits - 1)
  val tagHits = tagCam.io.hitBits & VecInit(ppnFlags.map { entry =>
    entry.dirty | !(io.req.bits.store & Mux(privS, entry.sWrite, entry.uWrite))}).asUInt
  val readHits = tagCam.io.hitBits & VecInit(ppnFlags.map { entry =>
    Mux(privS, entry.sRead, entry.uRead)}).asUInt
  val writeHits = tagCam.io.hitBits & VecInit(ppnFlags.map { entry =>
    Mux(privS, entry.sWrite, entry.uWrite)}).asUInt
  val execHits = tagCam.io.hitBits & VecInit(ppnFlags.map { entry =>
    Mux(privS, entry.sExecute, entry.uExecute)}).asUInt
  val tagHit = tagHits.orR
  val tlbHit = tagHit & vmEnable
  val tlbMiss = vmEnable & !tagHit & !badVA

  val refillAddr = Reg(UInt(camAddrBits.W))
  val refillTag = Reg(UInt(camTagBits.W))
  val req = Reg(new TLBReq)
  val pte = io.ptw.resp.bits.pte
  val error = io.ptw.resp.bits.error
  tagCam.io.write := io.ptw.resp.valid && state === sWait
  tagCam.io.writeAddr := refillAddr
  tagCam.io.writeTag := refillTag
  when (io.ptw.resp.valid && state === sWait) {
    ppnRam(refillAddr) := pte.ppn
    val entry = ppnFlags(refillAddr)
    entry.valid := !error
    entry.uRead := pte.uRead && !error
    entry.uWrite := pte.uWrite && !error
    entry.uExecute := pte.uExecute && !error
    entry.sRead := pte.sRead && !error
    entry.sWrite := pte.sWrite && !error
    entry.sExecute := pte.sExecute && !error
    entry.dirty := pte.dirty
  }

  val invalidEntry = PriorityEncoder(~tagCam.io.validBits)
  when (state === sReady && nextState === sRequest) {
    refillAddr := Mux(tagCam.io.validBits.andR, plru.replace, invalidEntry)
    refillTag := Cat(io.req.bits.asid, io.req.bits.vpn(vpnBits - 1, 0))
    req := io.req.bits
  }
  when (io.req.fire && tlbHit) {
    plru.access(OHToUInt(tagCam.io.hitBits))
  }

  io.req.ready := state === sReady
  io.resp.xcptLD := badVA || (tlbHit && !readHits.orR)
  io.resp.xcptST := badVA || (tlbHit && !writeHits.orR)
  io.resp.xcptIF := badVA || (tlbHit && !execHits.orR)
  io.resp.miss := tlbMiss
  io.resp.ppn := Mux(io.req.bits.passthrough | !vmEnable,
    io.req.bits.vpn(ppnBits.min(vpnBits) - 1, 0), ppnRam(OHToUInt(tagCam.io.hitBits)))
  io.resp.hitIdx := tagCam.io.hitBits

  tagCam.io.clear := io.req.fire | io.ptw.invalidate
  tagCam.io.clearBits := Mux(io.ptw.invalidate, (-1).S.asUInt, VecInit(ppnFlags.map(!_.valid)).asUInt |
    (VecInit(ppnFlags.map(entry =>
      !entry.dirty & (io.req.bits.store & Mux(privS, entry.sWrite, entry.uWrite)))).asUInt
    & tagCam.io.hitBits))

  io.ptw.req.valid := state === sRequest
  io.ptw.req.bits.addr := refillTag
  io.ptw.req.bits.prv := io.ptw.status.prv
  io.ptw.req.bits.store := req.store
  io.ptw.req.bits.fetch := req.instruction

  switch (state) {
    is (sReady) {
      when (io.req.fire && tlbMiss) {
        nextState := sRequest
      }
    }
    is (sRequest) {
      when (io.ptw.invalidate) {
        nextState := Mux(io.ptw.req.ready, sWaitInvalidate, sReady)
      } .elsewhen(io.ptw.req.ready) {
          nextState := sWait
      }
    }
    is (sWaitInvalidate) {
      when (io.ptw.resp.valid) {
        nextState := sReady
      }
    }
    is (sWait) {
      when (io.ptw.invalidate) {
        nextState := sWaitInvalidate
      } .elsewhen(io.ptw.resp.valid) {
        nextState := sReady
      }
    }
  }
}

object TLB extends App {
  implicit val p: Parameters = Parameters ((site, here, up) => {
    case CoreKey => SimpleCoreParams
    case CacheKey => SimpleCacheParams
    case TLBEntries => 32
  })

  generate(new TLB()(p))
}
