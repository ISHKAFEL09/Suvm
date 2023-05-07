package rocket2

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.BitPat.bitPatToUInt
import config._
import rocket2.tilelink._
import rocket2.tilelink.MemoryOpConstants._
import rocket2.util.Serializer

case object WordBits extends Field[Int]
case object StoreDataQueueDepth extends Field[Int]
case object ReplayQueueDepth extends Field[Int]
case object NMSHRs extends Field[Int]
case object LRSCCycles extends Field[Int]

trait HasL1DCacheParams extends HasL1CacheParams {
  val wordBits: Int = p(WordBits)
  val wordBytes: Int = wordBits / 8
  val wordOffBits: Int = log2Up(wordBytes)
  val sdqDepth: Int = p(StoreDataQueueDepth)
  val rowWords: Int = rowBits / wordBits
  val nMSHRs: Int = p(NMSHRs)
  val encDataBits: Int = code.width(coreDataBits)
  val encRowBits = encDataBits * rowWords
}

abstract class L1DCacheBundle(implicit val p: Parameters) extends Bundle with HasL1DCacheParams
abstract class L1DCacheModule(implicit val p: Parameters) extends Module with HasL1DCacheParams

trait HasCoreMemOp extends HasCoreParams {
  val addr: UInt = UInt(coreMaxAddrBits.W)
  val tag: UInt = UInt(coreDCacheRegTagBits.W)
  val cmd: UInt = UInt(M_SZ.W)
  val typ: UInt = UInt(MT_SZ.W)
}

trait HasCoreData extends HasCoreParams {
  val data: UInt = UInt(coreDataBits.W)
}

trait HasSDQId extends HasL1DCacheParams {
  val sdqId: UInt = UInt(log2Up(sdqDepth).W)
}

trait DCacheReqInternal extends HasCoreMemOp {
  val kill: Bool = Bool()
  val phys: Bool = Bool()
}

class L1MetaData(implicit p: Parameters) extends Metadata()(p) with HasL1DCacheParams {
  val coh = new ClientMetadata
}

object L1MetaData {
  def apply(tag: UInt, coh: ClientMetadata)(implicit p: Parameters): L1MetaData = {
    val meta = new L1MetaData()
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class L1MetaReadReq(implicit p: Parameters) extends MetaReadReq()(p) {
  val tag: UInt = UInt(tagBits.W)
}

class L1MetaWriteReq(implicit p: Parameters) extends MetaWriteReq[L1MetaData](new L1MetaData)(p)

class L1DataReadReq(implicit p: Parameters) extends L1DCacheBundle {
  val wayEn: UInt = UInt(nWays.W)
  val addr: UInt = UInt(untagBits.W)
}

class L1RefillReq(implicit p: Parameters) extends L1DataReadReq

class L1DataWriteReq(implicit p: Parameters) extends L1DataReadReq {
  val wmask: UInt = UInt(rowWords.W)
  val data: UInt = UInt(encRowBits.W)
}

trait HasMissInfo extends HasL1DCacheParams {
  val tagMatch: Bool = Bool()
  val oldMeta = new L1MetaData
  val wayEn: UInt = UInt(nWays.W)
}

class L1DCacheReq(implicit val p: Parameters) extends Bundle
  with DCacheReqInternal with HasCoreData with HasSDQId

class L1DCacheResp(implicit val p: Parameters) extends Bundle
  with HasCoreMemOp with HasCoreData {
  val nack: Bool = Bool()
  val replay: Bool = Bool()
  val hasData: Bool = Bool()
  val dataSubword: UInt = UInt(coreDataBits.W)
  val storeData: UInt = UInt(coreDataBits.W)
}

class MSHRReq(implicit p: Parameters) extends L1DCacheReq with HasMissInfo

class WritebackReq(implicit p: Parameters) extends Release with HasL1DCacheParams {
  val wayEn: UInt = UInt(nWays.W)
}

class CPU2L1DCacheIO(implicit p: Parameters) extends CoreBundle {
  val req = DecoupledIO(new L1DCacheReq)
  val resp = Flipped(ValidIO(new L1DCacheResp))
  val replayNext = Flipped(ValidIO(UInt(coreDCacheRegTagBits.W)))
  val invalidLR = Output(Bool())
  val ordered = Input(Bool())
}

object MSHR {
  object fsmStates extends ChiselEnum {
    val sInvalid, sWbReq, sWbResp = Value
    val sRefillReq, sRefillResp = Value
    val sMetaClear, sMetaWriteReq, sMetaWriteResp = Value
    val sDrainRpq: fsmStates.Type = Value
  }
}

class MSHR(val id: Int)(implicit p: Parameters) extends L1DCacheModule()(p) {
  val io = FlatIO(new Bundle {
    val priReq: DecoupledIO[MSHRReq] = Flipped(DecoupledIO(new MSHRReq))
    val secReq: DecoupledIO[MSHRReq] = Flipped(DecoupledIO(new MSHRReq))

    val idxMatch: Bool = Output(Bool())
    val tag: UInt = Output(UInt(tagBits.W))
    val probeReady: Bool = Output(Bool())

    val memReq = DecoupledIO(new Acquire)
    val refill: L1RefillReq = Output(new L1RefillReq)
    val memResp: ValidIO[Grant] = Flipped(ValidIO(new Grant))
    val wbReq = DecoupledIO(new WritebackReq)
    val metaWrite = DecoupledIO(new L1MetaWriteReq)
    val metaRead = DecoupledIO(new L1MetaReadReq)
    val replay = DecoupledIO(new L1DCacheReq)
  })

  import MSHR.fsmStates._
  val state: MSHR.fsmStates.Type = RegInit(sInvalid)
  val nextState: MSHR.fsmStates.Type = WireDefault(state)
  state := nextState

  val req: MSHRReq = RegEnable(io.priReq.bits, io.priReq.fire)
  val reqIdx: UInt = req.addr(untagBits - 1, blockOffBits)
  io.tag := req.addr >> untagBits
  io.idxMatch := state =/= sInvalid && reqIdx === io.secReq.bits.addr(untagBits - 1, blockOffBits)

  val nextCoh: ClientMetadata = RegNext(MuxCase(ClientMetadata.onReset, Seq(
    (state === sInvalid && nextState === sMetaWriteReq) ->
      io.priReq.bits.oldMeta.coh.onHit(io.priReq.bits.cmd),
    (state === sRefillReq && nextState === sMetaWriteReq) ->
      req.oldMeta.coh.onGrant(io.memResp.bits, req.cmd)
  )))

  val hasData: Bool = io.memResp.bits.hasMultibeatData()
  val (refillCount, refillCountDone) = Counter(io.memResp.fire && hasData, refillCycles)
  val refillDone: Bool = io.memResp.fire && (refillCountDone || !hasData)
  io.refill.addr := (if (refillCycles > 1) Cat(reqIdx, refillCount) else reqIdx) << rowOffBits
  io.refill.wayEn := req.wayEn

  val rpq: Queue[L1DCacheReq] = Module(new Queue(new L1DCacheReq, p(ReplayQueueDepth)))
  rpq.io.enq := 0.U.asTypeOf(new L1DCacheReq)
  rpq.io.deq.ready := io.replay.ready && io.metaRead.ready
  when (io.priReq.fire && !isPrefetch(io.priReq.bits.cmd)) {
    rpq.io.enq.valid := true.B
    rpq.io.enq.bits := io.priReq.bits
  } .elsewhen(io.secReq.fire && !isPrefetch(io.secReq.bits.cmd)) {
    rpq.io.enq.valid := true.B
    rpq.io.enq.bits := io.secReq.bits
  }

  val reqSecAcq: Bool = req.oldMeta.coh.requiresAcquireOnSecondaryMiss(req.cmd, io.secReq.bits.cmd)
  val statesBeforeRefill: Seq[MSHR.fsmStates.Type] = Seq(sWbReq, sWbResp, sMetaClear)
  val secReady: Bool = io.idxMatch &&
    (statesBeforeRefill.contain(state) || (Seq(sRefillReq, sRefillResp).contain(state) && !reqSecAcq))
  io.priReq.ready := state === sInvalid
  io.secReq.ready := rpq.io.enq.ready && secReady
  when (io.secReq.fire && reqSecAcq) {
    req.cmd := io.secReq.bits.cmd
  }

  // fsm
  switch(state) {
    is(sInvalid) {
      when(io.priReq.fire) {
        when(io.priReq.bits.tagMatch) {
          /* no need to replace block */
          when(io.priReq.bits.oldMeta.coh.isHit(io.priReq.bits.cmd)) {
            /* override meta directly */
            nextState := sMetaWriteReq
          } otherwise {
            /* acquire permission and refill block */
            nextState := sRefillReq
          }
        } otherwise {
          /* release and refill old meta&data */
          nextState := Mux(io.priReq.bits.oldMeta.coh.requiresVoluntaryWriteback(), sWbReq, sMetaClear)
        }
      }
    }
    is(sWbReq) {
      when(io.wbReq.fire) {
        nextState := Mux(io.wbReq.bits.requiresAck(), sWbResp, sMetaClear)
      }
    }
    is(sWbResp) {
      when(io.memResp.fire) {
        nextState := sMetaClear
      }
    }
    is(sMetaClear) {
      when(io.metaWrite.fire) {
        nextState := sRefillReq
      }
    }
    is(sRefillReq) {
      when(io.memReq.fire) {
        nextState := sRefillResp
      }
    }
    is(sRefillResp) {
      when(refillDone) {
        nextState := sMetaWriteReq
      }
    }
    is(sMetaWriteReq) {
      when(io.metaWrite.fire) {
        nextState := sMetaWriteResp
      }
    }
    is(sMetaWriteResp) {
      nextState := sDrainRpq
    }
    is(sDrainRpq) {
      when(!rpq.io.deq.valid) {
        nextState := sInvalid
      }
    }
  }

  io.wbReq.valid := state === sWbReq
  io.wbReq.bits := req.oldMeta.coh.makeVoluntaryWriteback(id.U, Cat(req.oldMeta.tag, reqIdx))
  io.wbReq.bits.wayEn := req.wayEn

  io.metaWrite.valid := state === sMetaClear || state === sMetaWriteReq
  io.metaWrite.bits.idx := reqIdx
  io.metaWrite.bits.wayEn := req.wayEn
  io.metaWrite.bits.data.tag := io.tag
  io.metaWrite.bits.data.coh :=
    Mux(state === sMetaClear, req.oldMeta.coh.onCacheControl(bitPatToUInt(M_FLUSH)), nextCoh)

  io.memReq.valid := state === sRefillReq
  io.memReq.bits := req.oldMeta.coh.makeAcquire(id.U, (req.addr >> blockOffBits).asUInt, req.cmd)

  io.metaRead.valid := state === sDrainRpq && rpq.io.deq.valid
  io.metaRead.bits.tag := io.tag
  io.metaRead.bits.idx := reqIdx
  io.replay.valid := state === sDrainRpq && rpq.io.deq.valid
  io.replay.bits := rpq.io.deq.bits
  io.replay.bits.phys := true.B

  val metaClearF: Bool = RegNext(io.metaWrite.fire)
  val metaClearFF: Bool = RegNext(metaClearF)
  io.probeReady := !io.idxMatch ||
    (!statesBeforeRefill.contain(state) && !metaClearF && !metaClearFF) // ensure no wb happen before probe
}

class MSHRFile(implicit p: Parameters) extends L1DCacheModule()(p) {
  val io = FlatIO(new Bundle {
    val req: DecoupledIO[MSHRReq] = Flipped(DecoupledIO(new MSHRReq))
    val memReq = DecoupledIO(new Acquire)
    val memResp: ValidIO[Grant] = Flipped(ValidIO(new Grant))
    val metaRead = DecoupledIO(new L1MetaReadReq)
    val metaWrite = DecoupledIO(new L1MetaWriteReq)
    val replay = DecoupledIO(new L1DCacheReq)
    val refill: L1RefillReq = Output(new L1RefillReq)
    val wbReq = DecoupledIO(new WritebackReq)

    val secMiss: Bool = Output(Bool())
    val probeReady: Bool = Output(Bool())
    val fenceReady: Bool = Output(Bool())
  })

  val mshrs = Seq.tabulate(nMSHRs)(i => Module(new MSHR(i)))
  val idxMatch: Bool = mshrs.map(_.io.idxMatch).reduce(_ || _)
  val secReady: Bool = mshrs.map(_.io.secReq.ready).reduce(_ || _)
  val tagMatch: Bool = mshrs.map { i =>
    i.io.idxMatch && (i.io.tag === (io.req.bits.addr >> untagBits).asUInt)
  }.reduce(_ || _)

  val sdq: Mem[UInt] = Mem(sdqDepth, io.req.bits.data)
  val sdqBitmap: Vec[Bool] = RegInit(VecInit.fill(sdqDepth)(true.B))
  val sdqReady: Bool = sdqBitmap.asUInt.orR
  val sdqAllocId: UInt = PriorityEncoder(sdqBitmap)
  val sdqWrEn: Bool = io.req.fire && isWrite(io.req.bits.cmd)
  when (sdqWrEn) {
    sdq.write(sdqAllocId, io.req.bits.data)
  }
  val sdqFreeId: UInt = io.replay.bits.sdqId
  val sdqRdEn: Bool = io.replay.fire && isWrite(io.replay.bits.cmd)
  when (sdqWrEn || sdqRdEn) {
    val allocMask = Mux(sdqWrEn, ~PriorityEncoderOH(sdqBitmap.asUInt), ~0.U(sdqDepth.W)).asUInt
    val freeMask = Mux(sdqRdEn, UIntToOH(sdqFreeId), 0.U(sdqDepth.W)).asUInt
    sdqBitmap := (sdqBitmap.asUInt & allocMask | freeMask).asBools
  }

  val allocArb = new Arbiter(new MSHRReq, nMSHRs)
  allocArb.io.out.ready := io.req.valid && sdqReady && !idxMatch
  io.req.ready := Mux(idxMatch, secReady && tagMatch, allocArb.io.out.valid) && sdqReady

  val wbArb = new Arbiter(new WritebackReq, nMSHRs)
  val metaWriteArb = new Arbiter(new L1MetaWriteReq, nMSHRs)
  val metaReadArb = new Arbiter(new L1MetaWriteReq, nMSHRs)
  val replayArb = new Arbiter(new L1DCacheReq, nMSHRs)
  val memReqArb = new Arbiter(new Acquire, nMSHRs)

  mshrs.foreach { mshr =>
    val i = mshr.id
    mshr.io.priReq.bits := io.req.bits
    mshr.io.secReq.bits := io.req.bits

    mshr.io.priReq.valid := allocArb.io.in(i).ready
    allocArb.io.in(i).valid := mshr.io.priReq.ready
    mshr.io.priReq.bits := io.req.bits
    mshr.io.priReq.bits.sdqId := sdqAllocId

    mshr.io.secReq.valid := io.req.valid && sdqReady && tagMatch
    mshr.io.secReq.bits := io.req.bits
    mshr.io.secReq.bits.sdqId := sdqAllocId

    mshr.io.wbReq <> wbArb.io.in(i)
    mshr.io.metaWrite <> metaWriteArb.io.in(i)
    mshr.io.metaRead <> metaReadArb.io.in(i)
    mshr.io.replay <> replayArb.io.in(i)
    mshr.io.memReq <> memReqArb.io.in(i)

    mshr.io.memResp.valid := io.memResp.valid && io.memResp.bits.client_xact_id === i.U
    mshr.io.memResp.bits := io.memResp.bits
  }

  wbArb.io.out <> io.wbReq
  metaWriteArb.io.out <> io.metaWrite
  metaReadArb.io.out <> io.metaRead
  replayArb.io.out <> io.replay
  memReqArb.io.out <> io.memReq

  io.refill := MuxLookup(io.memResp.bits.client_xact_id, 0.U, mshrs.map(mshr => (mshr.id.U, mshr.io.refill)))
  io.secMiss := mshrs.map(_.io.idxMatch).reduce(_ || _)
  io.probeReady := mshrs.map(_.io.probeReady).reduce(_ && _)
  io.fenceReady := mshrs.map(_.io.priReq.ready).reduce(_ && _)
  when (sdqRdEn) {
    io.replay.bits.data := RegNext {
      sdq.read(replayArb.io.out.bits.sdqId)
    }
  }
}

class WriteBackUnit(implicit p: Parameters) extends L1DCacheModule()(p) {
  val io = IO(new Bundle {
    val req: DecoupledIO[WritebackReq] = Flipped(DecoupledIO(new WritebackReq))
    val metaRead = DecoupledIO(new L1MetaReadReq)
    val dataRead = DecoupledIO(new L1DataReadReq)
    val dataResp: UInt = Input(UInt(encRowBits.W))
    val release = DecoupledIO(new Release)
  })

  val req: DecoupledIO[WritebackReq] = RegInit(0.U.asTypeOf(io.req))
  val addr: UInt = RegInit(0.U(log2Up(refillCycles + 1).W))
  val beatBitmap: UInt = if (refillCyclesPerBeat > 1) RegInit(0.U((refillCyclesPerBeat - 1).W)) else 1.U
  val buffer: Vec[UInt] = RegInit(VecInit.fill(refillCyclesPerBeat - 1)(0.U(encRowBits.W)))
  val beatDone: Bool = beatBitmap.andR
  val s1Valid: Bool = RegInit(false.B)
  val s2Valid: Bool = RegNext(s1Valid)
  val active: Bool = RegInit(false.B)

  io.req.ready := !active
  when (io.req.fire) {
    active := true.B
    req := io.req
    addr := 0.U
    if (refillCyclesPerBeat > 1) beatBitmap := 0.U
  }

  val idx: UInt = req.bits.addr_block(idxBits - 1, 0)
  val valid: Bool = active && (addr < refillCycles.U)
  io.metaRead.valid := valid
  io.metaRead.bits.idx := idx
  io.metaRead.bits.tag := req.bits.addr_block >> idxBits

  io.dataRead.valid := valid
  io.dataRead.bits.addr := (if (refillCycles > 1) Cat(idx, addr) else idx) << rowOffBits
  io.dataRead.bits.wayEn := req.bits.wayEn

  val readValid: Bool = io.metaRead.fire && io.dataRead.fire
  s1Valid := readValid
  when (readValid) {
    addr := addr + 1.U
  }
  when (s2Valid) {
    when (beatDone) {
      when (!io.release.ready) {
        s1Valid := false.B
        s2Valid := false.B
        addr := addr - Mux((refillCycles > 1).B && s1Valid, 2.U, 1.U)
      } otherwise {
        if (refillCyclesPerBeat > 1) beatBitmap := 0.U
      }
    } otherwise {
      if (refillCyclesPerBeat > 1) {
        for (i <- 0 until refillCyclesPerBeat - 2) {
          buffer(i) := buffer(i + 1)
        }
        buffer(refillCyclesPerBeat - 2) := io.dataResp
        beatBitmap := Cat(true.B, beatBitmap(refillCyclesPerBeat - 2, 1))
      }
    }
    active := s1Valid || !io.release.ready
  }

  val (addrBeat, _) = Counter(io.release.fire, outerDataBeats)
  io.release.valid := s2Valid && beatDone
  io.release.bits := req
  io.release.bits.addr_beat := addrBeat
  io.release.bits.data := (if (refillCyclesPerBeat > 1) Cat(io.dataResp, buffer.asUInt) else io.dataResp)
}

object ProbeUnit {
  object fsmStates extends ChiselEnum {
    val sInvalid, sMetaRead, sMetaResp, sCheck, sRelease, sWbReq, sWbResp, sMetaWrite = Value
  }
}

class ProbeUnit(implicit p: Parameters) extends L1DCacheModule()(p) {
  val io = IO(new Bundle {
    val probe: DecoupledIO[Probe] = Flipped(DecoupledIO(new Probe))
    val release = DecoupledIO(new Release)
    val metaRead = DecoupledIO(new L1MetaReadReq)
    val metaWrite = DecoupledIO(new L1MetaWriteReq)
    val wbReq = DecoupledIO(new WritebackReq)
    val wayEn: UInt = Input(UInt(nWays.W))
    val mshrRdy: Bool = Input(Bool())
    val coh: ClientMetadata = Input(new ClientMetadata)
  })

  import ProbeUnit.fsmStates._
  val state: ProbeUnit.fsmStates.Type = RegInit(sInvalid)
  val nextState: ProbeUnit.fsmStates.Type = WireDefault(state)
  state := nextState

  val probe: Probe = RegEnable(io.probe.bits, io.probe.fire)
  val coh: ClientMetadata = RegEnable(io.coh, state === sCheck)
  val wayEn: UInt = RegEnable(io.wayEn, state === sCheck)
  val tagMatch: Bool = wayEn.orR

  switch (state) {
    is (sInvalid) {
      when (io.probe.fire) {
        nextState := sMetaRead
      }
    }
    is (sMetaRead) {
      when (io.metaRead.fire) {
        nextState := sMetaResp
      }
    }
    is (sMetaResp) {
      nextState := sCheck
    }
    is (sCheck) {
      nextState := Mux(io.mshrRdy, sRelease, sMetaRead)
    }
    is (sRelease) {
      nextState := sInvalid
      when (tagMatch) {
        when (coh.requiresVoluntaryWriteback()) {
          nextState := sWbReq
        } .elsewhen(io.release.fire) {
          nextState := sMetaWrite
        }
      }
    }
    is (sWbReq) {
      when (io.wbReq.fire) {
        nextState := sWbResp
      }
    }
    is (sWbResp) {
      when (io.wbReq.ready) {
        nextState := sMetaWrite
      }
    }
    is (sMetaWrite) {
      when (io.metaWrite.fire) {
        nextState := sInvalid
      }
    }
  }

  io.probe.ready := state === sInvalid

  io.metaRead.valid := state === sMetaRead
  io.metaRead.bits.idx := probe.addr_block(idxBits - 1, 0)
  io.metaRead.bits.tag := probe.addr_block >> idxBits

  io.release.valid := state === sRelease && !(tagMatch && coh.requiresVoluntaryWriteback())
  io.release.bits := coh.makeRelease(probe)

  io.wbReq.valid := state === sWbReq
  io.wbReq.bits := coh.makeRelease(probe)
  io.wbReq.bits.wayEn := wayEn

  io.metaWrite.valid := state === sMetaWrite
  io.metaWrite.bits.wayEn := wayEn
  io.metaWrite.bits.idx := probe.addr_block(idxBits - 1, 0)
  io.metaWrite.bits.data.tag := probe.addr_block >> idxBits
  io.metaWrite.bits.data.coh := coh.onProbe(probe)
}

class DataArray(implicit p: Parameters) extends L1DCacheModule()(p) {
  val io = IO(new Bundle {
    val read: DecoupledIO[L1DataReadReq] = Flipped(DecoupledIO(new L1DataReadReq))
    val write: DecoupledIO[L1DataWriteReq] = Flipped(DecoupledIO(new L1DataWriteReq))
    val resp: Vec[UInt] = Output(VecInit.fill(nWays)(UInt(encRowBits.W)))
  })

  val wrAddr: UInt = (io.write.bits.addr >> rowOffBits).asUInt
  val rdAddr: UInt = (io.read.bits.addr >> rowOffBits).asUInt

  val mem = Seq.fill(nWays)(SyncReadMem(nSets * refillCycles, Vec(rowWords, UInt(encDataBits.W))))
  for ((m, i) <- mem.zipWithIndex) {
    when (io.write.bits.wayEn(i) && io.write.fire) {
      val data = VecInit.tabulate(rowWords){ i =>
        io.write.bits.data(encDataBits * (i + 1) - 1, encDataBits * i)
      }
      m.write(wrAddr, data, io.write.bits.wmask.asBools)
    }
    io.resp(i) := m.read(rdAddr, io.read.bits.wayEn(i) && io.read.fire)
  }
  io.read.ready := true.B
  io.write.ready := true.B
  when (io.read.fire && io.write.fire) {
    assert(wrAddr =/= rdAddr)
  }
}

class NBDCache(implicit p: Parameters) extends L1DCacheModule()(p) {
  val io = IO(new Bundle {
    val cpu = Flipped(new CPU2L1DCacheIO)
    val ptw = new TLB2PTWIO()
    val mem = new ClientTileLinkIO
  })

  // modules delcare
  val mshrs = Module(new MSHRFile())
  val wb = Module(new WriteBackUnit())
  val prober = Module(new ProbeUnit())

  // meta
  val meta = Module(new MetaArray(() => L1MetaData(0.U, ClientMetadata.onReset)))
  val metaReadArb = Module(new Arbiter(new MetaReadReq, 5))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  metaReadArb.io.out <> meta.io.read
  metaWriteArb.io.out <> meta.io.write

  // data
  val data = Module(new DataArray)
  val dataReadArb = Module(new Arbiter(new L1DataReadReq, 4))
  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, 2))
  dataReadArb.io.out <> data.io.read
  dataWriteArb.io.out <> data.io.write
  data.io.write.bits.data := VecInit.tabulate(rowWords) { i =>
    code.encode(dataWriteArb.io.out.bits.data(coreDataBits * (i + 1) - 1, coreDataBits * i))
  }.asUInt

  // read meta & data
  metaReadArb.io.in(4).valid := io.cpu.req.valid
  metaReadArb.io.in(4).bits.idx := io.cpu.req.bits.addr >> blockOffBits
  when (!metaReadArb.io.in(4).ready) {
    io.cpu.req.ready := false.B
  }
  dataReadArb.io.in(3).valid := io.cpu.req.valid
  dataReadArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  dataReadArb.io.in(3).bits.wayEn := (-1).S
  when (!dataReadArb.io.in(3).ready) {
    io.cpu.req.ready := false.B
  }

  // s1 begin
  io.cpu.req.ready := true.B
  val s1Valid = RegNext(io.cpu.req.fire, init = false.B)
  val s1Replay = RegInit(false.B)
  val s1Req = RegEnable(io.cpu.req.bits, io.cpu.req.fire)
  when (wb.io.metaRead.fire) {
    s1Req.addr := Cat(wb.io.metaRead.bits.tag, wb.io.metaRead.bits.idx) << blockOffBits
    s1Req.phys := true.B
  }
  when(prober.io.metaRead.fire) {
    s1Req.addr := Cat(prober.io.metaRead.bits.tag, prober.io.metaRead.bits.idx) << blockOffBits
    s1Req.phys := true.B
  }
  when(mshrs.io.replay.fire) {
    s1Req := mshrs.io.replay.bits
  }
  val s1ValidMasked = s1Valid & !io.cpu.req.bits.kill
  val s1MetaValid = RegNext(metaReadArb.io.out.fire)
  // cmd decode
  val s1Read = isRead(s1Req.cmd)
  val s1Write = isWrite(s1Req.cmd)
  val s1SC = s1Req.cmd === M_XSC
  val s1RW = s1Read | s1Write | isPrefetch(s1Req.cmd)

  // vpn -> ppn
  val dtlb = Module(new TLB)
  dtlb.io.ptw <> io.ptw
  dtlb.io.req.valid := s1ValidMasked & s1RW & !s1Req.phys
  dtlb.io.req.bits.passthrough := s1Req.phys
  dtlb.io.req.bits.asid := 0.U
  dtlb.io.req.bits.vpn := s1Req.addr >> pgIdxBits
  dtlb.io.req.bits.instruction := false.B
  dtlb.io.req.bits.store := s1Write
  when (!dtlb.io.req.ready & !io.cpu.req.bits.phys) {
    io.cpu.req.ready := false.B
  }
  val s1Addr = Cat(dtlb.io.resp.ppn, s1Req.addr(pgIdxBits - 1, 0))

  // s1 tag check
  def wayMap[T <: Data](f: Int => T): Vec[T] = VecInit.tabulate(nWays)(f)
  val s1State = wayMap(i => meta.io.resp(i).coh)
  val s1TagMatchWay = wayMap { i =>
    (meta.io.resp(i).tag === (s1Addr >> untagBits).asUInt) & s1State(i).isValid()
  }

  // s2 begin
  val s2Valid = RegNext(s1ValidMasked, init = false.B)
  val s2ValidMasked = Wire(Bool())
  val s2Req = RegEnable(s1Req, s1MetaValid)
  s2Req.addr := s1Addr
  when (s1Write) {
    s2Req.data := Mux(s1Replay, mshrs.io.replay.bits.data, io.cpu.req.bits.data)
  }
  val s2Replay = RegNext(s1Replay, init = false.B) && (s2Req.cmd =/= M_NOP)
  val s2Recycle = Wire(Bool())
  val s1Recycle = RegEnable(s2Recycle, s1MetaValid)
  when (s1Recycle) {
    s2Req.data := s1Req.data
  }

  // s2 tag check
  val s2State = RegEnable(s1State, s1MetaValid)
  val s2TagMatchWay = RegEnable(s1TagMatchWay, s1MetaValid)
  val s2TagMatch = s2TagMatchWay.reduce(_ | _)
  val s2HitState = Mux1H(s2TagMatchWay, s2State)
  val s2Hit = s2TagMatch && s2HitState.isHit(s2Req.cmd) && s2HitState === s2HitState.onHit(s2Req.cmd)

  // lr/sc
  val lrscCount = RegInit(0.U)
  val lrscValid = lrscCount.orR
  val lrscAddr = RegInit(0.U)
  val lrscMatch = lrscAddr === (s2Req.addr >> blockOffBits)
  val s2LrscFail = (s2Req.cmd === M_XSC) && !(lrscValid && lrscMatch)
  when (lrscValid) {
    lrscCount := lrscCount - 1.U
  }
  when (s2Hit && s2ValidMasked || s2Replay) {
    when (s2Req.cmd === M_XLR) {
      when (!lrscValid) {
        lrscCount := (p(LRSCCycles) - 1).U
      }
      lrscAddr := s2Req.addr >> blockOffBits
    }
    when (s2Req.cmd === M_XSC) {
      lrscCount := 0.U
    }
  }
  when (io.cpu.invalidLR) {
    lrscCount := 0.U
  }

  // s2 data
  val s2Data = RegInit(0.U.asTypeOf(VecInit.fill(nWays)(UInt(encRowBits.W))))
  for (w <- 0 until nWays) {
    when (s1MetaValid && s1TagMatchWay(w)) {
      s2Data(w) := data.io.resp(w)
    }
  }
  val s2DataMatch = Mux1H(s2TagMatchWay, s2Data)
  val s2DataDecoded = (0 until rowWords).map { i =>
    code.decode(s2DataMatch(encDataBits * (i + 1) - 1, encDataBits * i))
  }
  val s2DataCorrected = VecInit.tabulate(rowWords)(s2DataDecoded(_).corrected).asUInt
  val s2DataUncorrected = VecInit.tabulate(rowWords)(s2DataDecoded(_).uncorrected).asUInt
  val s2WordIdx = s2Req.addr(log2Up(rowWords * coreDataBits) - 1, log2Up(wordBytes))
  val s2DataCorrectable = VecInit.tabulate(rowWords)(s2DataDecoded(_).correctable)(s2WordIdx)

  // recycle
  val s2RecycleEcc = (s2Valid || s2Replay) && s2Hit && s2DataCorrectable
  val s2RecycleNext = RegNext((s1Valid || s1Replay) && s2RecycleEcc) // recycle all cmd after ecc cmd
  s2Recycle := s2RecycleEcc || s2RecycleNext
  when(s2Recycle) {
    s1Req := s2Req
  }
  metaReadArb.io.in(0).valid := s2Recycle
  metaReadArb.io.in(0).bits.idx := s2Req.addr >> blockOffBits
  dataReadArb.io.in(0).valid := s2Recycle
  dataReadArb.io.in(0).bits.addr := s2Req.addr
  dataReadArb.io.in(0).bits.wayEn := (-1).S

  // s3 store/amo hits
  val amoAlu = Module(new AMOALU)
  val s3ValidComb = (s2ValidMasked && s2Hit || s2Replay) && !s2LrscFail && (isWrite(s2Req.cmd) || s2DataCorrectable)
  val s3Valid = RegNext(s3ValidComb, init = false.B)
  val s3Req = RegEnable(s2Req, s3ValidComb)
  val s3Way = RegEnable(s2TagMatchWay, s3ValidComb)
  s3Req.data := Mux(s2DataCorrectable, s2DataCorrected, amoAlu.io.out)

  val rowIdx = s3Req.addr(rowOffBits - 1, wordOffBits)
  val rowMask = 1.U << rowIdx
  dataWriteArb.io.in(0).valid := s3Valid
  dataWriteArb.io.in(0).bits.data := Fill(rowWords, s3Req.data)
  dataWriteArb.io.in(0).bits.wmask := rowMask
  dataWriteArb.io.in(0).bits.wayEn := s3Way

  // s4 store
  val s4Valid = RegNext(s3Valid, init = false.B)
  val s4Req = RegEnable(s3Req, s3Valid)

  // s2 load
  val bypasses = Seq(
    ((s2ValidMasked || s2Replay) && !s2LrscFail, s2Req, amoAlu.io.out),
    (s3Valid, s3Req, s3Req.data),
    (s4Valid, s4Req, s4Req.data),
  ).map { case (valid, req, data) =>
    (valid && (s1Addr >> wordOffBits === req.addr >> wordOffBits) && isWrite(req.cmd), data)
  }
  val s2BypassValid = RegInit(false.B)
  val s2BypassData = RegInit(0.U(coreDataBits.W))
  when (s1MetaValid) {
    s2BypassValid := false.B
    when (bypasses.map(_._1).reduce(_ || _)) {
      s2BypassValid := true.B
      s2BypassData := PriorityMux(bypasses)
    }
  }
  val s2DataPrim = s2DataUncorrected >> Cat(s2WordIdx, 0.U(coreDataBits.W))
  val s2DataWord = Mux(s2BypassValid, s2BypassData, s2DataPrim).asUInt
  val loadGen = new LoadGen(s2Req.typ, s2Req.addr, s2DataWord, s2Req.cmd === M_XSC)
  amoAlu.io := s2Req
  amoAlu.io.lhs := s2DataWord
  amoAlu.io.rhs := s2Req.data

  // s2 miss
  val replacer = p(Replacer)()
  val s1ReplaceWay = UIntToOH(replacer.way)
  val s2ReplaceWay = RegEnable(s1ReplaceWay, s1MetaValid)
  val s2ReplaceMeta = Mux1H(s2ReplaceWay, wayMap { i =>
    RegEnable(meta.io.resp(i), s1MetaValid && s1ReplaceWay(i))
  })
  // req -> mshr
  mshrs.io.req.valid := s2ValidMasked && !s2Hit
  mshrs.io.req.bits := s2Req
  mshrs.io.req.bits.tagMatch := s2TagMatch
  mshrs.io.req.bits.oldMeta := Mux(s2TagMatch, L1MetaData(s2ReplaceMeta.tag, s2HitState), s2ReplaceMeta)
  mshrs.io.req.bits.wayEn := Mux(s2TagMatch, s2TagMatchWay, s2ReplaceWay)
  when (mshrs.io.req.fire) {
    replacer.miss
  }
  mshrs.io.memReq <> io.mem.acquire
  // memResp -> dcache
  val granter = Serializer(io.mem.grant, refillCyclesPerBeat)
  mshrs.io.memResp.valid := granter.fire
  mshrs.io.memResp.bits := granter.bits
  granter.ready := dataWriteArb.io.in(1).ready || !granter.bits.hasData()
  dataWriteArb.io.in(1).valid := granter.valid && granter.bits.hasData()
  dataWriteArb.io.in(1).bits.addr := mshrs.io.refill.addr
  dataWriteArb.io.in(1).bits.data := granter.bits.data
  dataWriteArb.io.in(1).bits.wayEn := mshrs.io.refill.wayEn
  dataWriteArb.io.in(1).bits.wmask := (-1).S
  // replay
  dataReadArb.io.in(1).valid := mshrs.io.replay.valid
  dataReadArb.io.in(1).bits := mshrs.io.replay.bits
  dataReadArb.io.in(1).bits.wayEn := (-1).S
  mshrs.io.replay.ready := dataReadArb.io.in(1).ready
  s1Replay := mshrs.io.replay.fire
  mshrs.io.metaRead <> metaReadArb.io.in(1)
  mshrs.io.metaWrite <> metaWriteArb.io.in(0)

  // release
  val releaseArb = Module(new LockingArbiter(new Release, 2, outerDataBeats,
    Some((r: Release) => r.hasMultibeatData())))
  releaseArb.io.out <> io.mem.release

  // wb
  val wbArb = Module(new Arbiter(new WritebackReq, 2))
  mshrs.io.wbReq <> wbArb.io.in(1)
  prober.io.wbReq <> wbArb.io.in(0)
  wbArb.io.out <> wb.io.req
  wb.io.metaRead <> metaReadArb.io.in(3)
  wb.io.dataRead <> dataReadArb.io.in(2)
  wb.io.dataResp := s2DataCorrected
  wb.io.release <> releaseArb.io.in(0)

  // probe
  prober.io.probe.valid := io.mem.probe.valid && !lrscValid
  io.mem.probe.ready := prober.io.probe.ready && !lrscValid
  prober.io.probe.bits := io.mem.probe.bits
  prober.io.wayEn := s2TagMatchWay
  prober.io.coh := s2HitState
  prober.io.mshrRdy := mshrs.io.probeReady
  prober.io.release <> releaseArb.io.in(1)
  prober.io.metaRead <> metaReadArb.io.in(2)
  prober.io.metaWrite <> metaWriteArb.io.in(1)

  // nack
  val s1NackDtlb = dtlb.io.req.valid && dtlb.io.resp.miss
  val s1NackProbe = !prober.io.probe.ready && 
    (s1Req.addr(untagBits - 1, blockOffBits) === prober.io.metaWrite.bits.idx)
  val s1Nack = s1NackDtlb || s1NackProbe
  val s2NackHit = RegEnable(s1Nack, s1Valid || s1Replay)
  when (s2NackHit) {
    mshrs.io.req.valid := false.B
  }
  val s2NackVictim = s2Hit && mshrs.io.secMiss
  val s2NackMiss = !s2Hit && !mshrs.io.req.ready
  val s2Nack = s2NackHit || s2NackVictim || s2NackMiss
  s2ValidMasked := s2Valid && !s2Nack

  // after a nack, block until nack condition resolves to save energy
  val blockMiss = RegInit(false.B)
  blockMiss := (s2Valid || blockMiss) && s2NackMiss
  when(blockMiss) {
    io.cpu.req.ready := false.B
  }

  // cpu response
  io.cpu.resp.valid := (s2Replay || (s2ValidMasked && s2Hit)) && !s2DataCorrectable
  io.cpu.resp.bits.nack := s2Valid && s2Nack
  io.cpu.resp.bits := s2Req
  io.cpu.resp.bits.hasData := isRead(s2Req.cmd) || (s2Req.cmd === M_XSC)
  io.cpu.resp.bits.replay := s2Replay
  io.cpu.resp.bits.data := loadGen.word
  io.cpu.resp.bits.dataSubword := loadGen.byte | s2LrscFail
  io.cpu.resp.bits.storeData := s2Req.data
  io.cpu.ordered := mshrs.io.fenceReady && !s1Valid && !s2Valid
  io.cpu.replayNext.valid := s1Replay && (s1Read || s1SC)
  io.cpu.replayNext.bits := s1Req.tag
}

object NBDCache extends App {
  implicit val p: Parameters = Parameters ((site, here, up) => {
    case CoreKey => SimpleCoreParams
    case CacheKey => SimpleCacheParams
    case TLDataBeats => 1
    case TLDataBits => 64
    case WordBits => 32
    case StoreDataQueueDepth => 32
    case NMSHRs => 4
    case TLCoherencePolicy => new MICoherence(new NullRepresentation(1))
    case TLNManagers => 1
    case TLNClients => 1
    case TLMaxClientXacts => 1
    case TLMaxClientsPerPort => 1
    case TLMaxManagerXacts => 1
    case TLBlockAddrBits => 6
    case TLDataBits => 64
    case TLDataBeats => 1
    case TLNetworkIsOrderedP2P => false
    case AmoAluOperandBits => 64
    case TLId => "nbdcache"
  })

  generate(new NBDCache()(p))
}