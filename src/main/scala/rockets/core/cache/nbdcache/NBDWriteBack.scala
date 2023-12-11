package rockets.core.cache.nbdcache

import rockets.params._
import rockets.params.config._
import rockets.tilelink._
import spinal.core._
import spinal.lib._

case class NBDWriteBackReq()(implicit p: Parameters)
    extends Release
    with HasDCacheParams {
  val way = UInt(nWays bits)
}

case class NBDWriteBackIO()(implicit p: Parameters)
    extends NBDBundle
    with IMasterSlave {
  val req = Stream(NBDWriteBackReq())
  val metaReq = Stream(NBDMetaReadReq())
  val dataReq = Stream(NBDDataReadReq())
  val dataResp = UInt(encRowBits bits)
  val release = Stream(new Release())

  override def asMaster(): Unit = {
    slave(metaReq, dataReq, release)
    master(req)
    out(dataResp)
  }
}

case class NBDWriteBack()(implicit p: Parameters) extends NBDComponent {
  val io = slave(NBDWriteBackIO())

  // global regs
  /** read valid pipelines */
  val readValidS0, readValidS1 = Reg(False)

  /** read cache address */
  val readAddr = Reg(UInt(log2Up(refillCycles + 1) bits)) init 0

  /** valid entries in buffer */
  val bufCnt = Counter(refillCyclesPerBeat)

  /** sent beats counter */
  val beatCnt = Counter(outerDataBeats, io.release.fire)
  val busy = RegInit(False) setWhen io.req.fire clearWhen beatCnt.willOverflow

  /** request buf */
  val req = RegNextWhen(io.req.payload, io.req.fire)
  val reqIdx = req.blockAddr(0, idxBits bits)
  val reqTag = req.blockAddr(idxBits, tagBits bits)

  // ctrl path
  /** read data from cache valid */
  val needRead = busy && readAddr < U(refillCycles)
  val readValid = io.metaReq.fire && io.dataReq.fire
  val updateBuf = !bufCnt.willOverflowIfInc && readValidS1
  val ready2Release = bufCnt.willOverflowIfInc && readValidS1
  val flush = ready2Release && !io.release.ready

  // data path
  /** pipeline */
  readValidS0 := readValid
  readValidS1 := readValidS0
  when(flush) {
    readValidS0 := False
    readValidS1 := False
  }

  /** update read address */
  when(readValid) {
    readAddr := readAddr + 1
  }
  when(flush) {
    readAddr := readAddr - 1 - readValidS0.asUInt
  }
  when(io.req.fire) {
    readAddr := 0
  }

  /** one beat data buffer, release when buffer nearly full */
  val buf = Vec(Reg(UInt(encRowBits bits)) init 0, refillCyclesPerBeat - 1)

  /** update buf */
  ifGen(refillCyclesPerBeat > 1) {
    when(updateBuf) {
      buf(bufCnt) := io.dataResp
    }
    when(updateBuf || io.release.fire) {
      bufCnt.increment()
    }
  }

  /** component io */
  io.req.ready := !busy

  io.metaReq.valid := needRead
  io.metaReq.payload.idx := reqIdx
  io.metaReq.payload.tag := reqTag

  io.dataReq.valid := needRead
  io.dataReq.payload.addr :=
    reqIdx @@ readAddr(0, log2Up(refillCycles) bits) @@ U(0, rowOffBits bits)
  io.dataReq.payload.way := req.way

  io.release.valid := ready2Release
  io.release.payload.allowOverride()
  io.release.payload := req
  io.release.payload.beatAddr := beatCnt
  io.release.payload.data := io.dataResp @@ buf.asBits.asUInt
}

object NBDWriteBack extends App {
  import rockets._
  import spinal.core.sim._
  import rockets.tile.Configs._

  val simv = SimConfig.withWave.compile(NBDWriteBack())

  simv.doSimUntilVoid("smoke") { dut =>
    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSampling(100)

    dut.clockDomain.onSamplings {
      dut.io.metaReq.ready.randomize()
      dut.io.dataReq.ready.randomize()
      dut.io.dataResp.randomize()
      dut.io.release.ready.randomize()
    }

    def sendReq(addr: BigInt): Unit = {
      dut.io.req.valid #= true
      dut.io.req.payload.blockAddr #= addr
      dut.clockDomain.waitSamplingWhere(dut.io.req.ready.toBoolean)
      dut.io.req.valid #= false
    }

    for (i <- 0 to 100) {
      sendReq(i)
    }

    simSuccess()
  }
}
