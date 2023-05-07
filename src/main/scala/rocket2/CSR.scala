package rocket2

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.FlatIO
import chisel3.util._
import config.Parameters
import PRV._
import rocket2.util.WideCounter

import scala.collection.mutable

class MStatus extends Bundle {
  val sd = Bool()
  val zero2 = UInt(31.W)
  val sdRv32 = Bool()
  val zero1 = UInt(9.W)
  val vm = UInt(5.W)
  val mPrv = Bool()
  val xs = UInt(2.W)
  val fs = UInt(2.W)
  val prv3 = UInt(2.W)
  val ie3 = Bool()
  val prv2 = UInt(2.W)
  val ie2 = Bool()
  val prv1 = UInt(2.W)
  val ie1 = Bool()
  val prv = UInt(2.W)
  val ie = Bool()
}

class SStatus extends Bundle {
  val sd = Bool()
  val zero4 = UInt(31.W)
  val sdRv32 = Bool()
  val zero3 = UInt(14.W)
  val mPrv = Bool()
  val xs = UInt(2.W)
  val fs = UInt(2.W)
  val zero2 = UInt(7.W)
  val ps = Bool()
  val pie = Bool()
  val zero1 = UInt(2.W)
  val ie = Bool()
}

class MIP extends Bundle {
  val mtip = Bool()
  val htip = Bool()
  val stip = Bool()
  val utip = Bool()
  val msip = Bool()
  val hsip = Bool()
  val ssip = Bool()
  val usip = Bool()
}

object CSR {
  val SZ = 3
  val N = 0.U(SZ.W)
  val W = 1.U(SZ.W)
  val S = 2.U(SZ.W)
  val C = 3.U(SZ.W)
  val I = 4.U(SZ.W)
  val R = 5.U(SZ.W)
}

class CSRFileIO(implicit p: Parameters) extends CoreBundle {
  val rw = new Bundle() {
    val addr = Input(UInt(12.W))
    val cmd = Input(UInt(CSR.SZ.W))
    val rdata = Output(UInt(xLen.W))
    val wdata = Input(UInt(xLen.W))
  }

  val rocc = Flipped(new RoCCIO)

  val hartId = Input(UInt(hartIdLen.W))
  val retire = Input(Bool())
  val exception = Input(Bool())
  val pc = Input(UInt(vAddrBitsExtended.W))
  val cause = Input(UInt(xLen.W))

  val interruptCause = Output(UInt(xLen.W))
  val interrupt = Output(Bool())
  val ptbr = Output(UInt(pAddrBits.W))
  val status = Output(new MStatus)
  val evec = Output(UInt(vAddrBitsExtended.W))
//  val csrReplay = Output(Bool())
  val csrStall = Output(Bool())
  val csrXcpt = Output(Bool())
  val eret = Output(Bool())
  val time = Output(UInt(xLen.W))
  val frm = Output(UInt(FPConstants.RM_SZ.W))
  val fflags = Flipped(ValidIO(UInt(FPConstants.FCMD_X.getWidth.W)))
}

class CSR(implicit p: Parameters) extends CoreModule {
  val io = FlatIO(new CSRFileIO)

  // M regs
  val regMstatus = RegInit(new MStatus().Lit(
    _.zero1 -> 0.U,
    _.zero2 -> 0.U,
    _.ie -> false.B,
    _.prv -> PRV_M.litValue.U,
    _.ie1 -> false.B,
    _.prv1 -> PRV_M.litValue.U,
    _.ie2 -> false.B,
    _.prv2 -> PRV_U.litValue.U,
    _.ie3 -> false.B,
    _.prv3 -> PRV_U.litValue.U,
    _.mPrv -> false.B,
    _.vm -> 0.U,
    _.fs -> 0.U,
    _.xs -> 0.U,
    _.sdRv32 -> false.B,
    _.sd -> false.B
  ))
  val regMie = RegInit(0.U.asTypeOf(new MIP))
  val regMip = RegInit(0.U.asTypeOf(new MIP))
  val regMepc = RegInit(0.U(vAddrBitsExtended.W))
  val regMcause = RegInit(0.U(xLen.W))
  val regMbadaddr = RegInit(0.U(vAddrBitsExtended.W))
  val regMscratch = RegInit(0.U(xLen.W))

  // S regs
  val regSepc = RegInit(0.U(vAddrBitsExtended.W))
  val regScause = RegInit(0.U(xLen.W))
  val regSbadaddr = RegInit(0.U(vAddrBitsExtended.W))
  val regSscratch = RegInit(0.U(xLen.W))
  val regStvec = RegInit(0.U(vAddrBits.W))
  val regStimecmp = RegInit(0.U(32.W))
  val regSptbr = RegInit(0.U(pAddrBits.W))
  val regWFI = RegInit(false.B)

  // U regs
  val regUtime = WideCounter(xLen)
  val regUinstret = WideCounter(xLen, io.retire)
  val regUfflags = RegInit(FPConstants.FCMD_X)
  val regUfrm = RegInit(0.U(FPConstants.RM_SZ.W))

  // irq
  val irqRoCC = p(BuildRoCC).nonEmpty.B && io.rocc.interrupt

  val interruptPending = WireInit(false.B)
  def checkInterrupt(causePrv: PRV.Type, cond: Bool, num: Int): Unit = {
    when (cond && (regMstatus.prv < causePrv.asUInt || (regMstatus.prv === causePrv.asUInt && regMstatus.ie))) {
      io.interruptCause := ((BigInt(1) << (xLen - 1)) + num).U
    }
    when (cond && regMstatus.prv <= causePrv.asUInt) {
      interruptPending := true.B
    }
  }
  io.interrupt := io.interruptCause(xLen - 1)
  io.interruptCause := 0.U
  checkInterrupt(PRV_S, regMie.ssip && regMip.ssip, 0)
  checkInterrupt(PRV_M, regMie.msip && regMip.msip, 0)
  checkInterrupt(PRV_S, regMie.stip && regMip.stip, 1)
  checkInterrupt(PRV_M, irqRoCC, 3)

  // csr reg map
  val systemInstr = io.rw.cmd === CSR.I
  val cpuRen = io.rw.cmd =/= CSR.N && !systemInstr
  val isa = "IMA" +
    (if (p(UseVM)) "S" else "") +
    (if (p(BuildFPU).nonEmpty) "FD" else "") +
    (if (p(BuildRoCC).nonEmpty) "X" else "")
  val cpuid = Cat(if (xLen == 32) 0.U(2.W) else 2.U(2.W), 0.U((xLen - 2).W)) |
    isa.map(x => 1 << (x - 'A')).reduce(_ | _).U(26.W)

  val csrMap = collection.mutable.HashMap[Int, UInt](
    CSRs.fflags -> (if (p(BuildFPU).nonEmpty) regUfflags.asUInt else 0.U),
    CSRs.frm -> (if (p(BuildFPU).nonEmpty) regUfrm.asUInt else 0.U),
    CSRs.fcsr -> (if (p(BuildFPU).nonEmpty) Cat(regUfrm, regUfflags.asUInt) else 0.U),
    CSRs.cycle -> regUtime.value,
    CSRs.cyclew -> regUtime.value,
    CSRs.instret -> regUinstret.value,
    CSRs.instretw -> regUinstret.value,
    CSRs.time -> regUtime.value,
    CSRs.timew -> regUtime.value,
    CSRs.stime -> regUtime.value,
    CSRs.stimew -> regUtime.value,
    CSRs.mcpuid -> cpuid,
    CSRs.mimpid -> 1.U,
    CSRs.mstatus -> io.status.asUInt,
    CSRs.mtdeleg -> 0.U,
    CSRs.mtvec -> MTVEC.U,
    CSRs.mip -> regMip.asUInt,
    CSRs.mie -> regMie.asUInt,
    CSRs.mscratch -> regMscratch,
    CSRs.mepc -> regMepc.asTypeOf(UInt(xLen.W)),
    CSRs.mbadaddr -> regMbadaddr.asTypeOf(UInt(xLen.W)),
    CSRs.mcause -> regMcause,
    CSRs.stimecmp -> regStimecmp,
    CSRs.mhartid -> HART_ID.U,
    CSRs.send_ipi -> HART_ID.U
  )

  if (p(UseVM)) {
    val sStatus = Wire(new SStatus)
    sStatus := io.status.asTypeOf(new SStatus)
    sStatus.zero1 := 0.U
    sStatus.zero2 := 0.U
    sStatus.zero3 := 0.U
    sStatus.zero4 := 0.U

    val sip = Wire(new MIP)
    sip := 0.U.asTypeOf(new MIP)
    sip.ssip := regMip.ssip
    sip.stip := regMip.stip

    val sie = Wire(new MIP)
    sie := 0.U.asTypeOf(new MIP)
    sie.ssip := regMie.ssip
    sie.stip := regMie.stip

    csrMap ++ mutable.HashMap(
      CSRs.sstatus -> sStatus.asUInt,
      CSRs.sip -> sip.asUInt,
      CSRs.sie -> sie.asUInt,
      CSRs.sscratch -> regSscratch,
      CSRs.scause -> regScause,
      CSRs.sbadaddr -> regSbadaddr.asTypeOf(UInt(xLen.W)),
      CSRs.sptbr -> regSptbr,
      CSRs.sasid -> 0.U,
      CSRs.sepc -> regSepc.asTypeOf(UInt(xLen.W)),
      CSRs.stvec -> regStvec.asTypeOf(UInt(xLen.W))
    )

    val csrDecoded = csrMap.map(reg => reg._1 -> (io.rw.addr === reg._1.U))
    val addrValid = csrDecoded.values.reduce(_ || _)
    val isFP = csrDecoded(CSRs.fflags) || csrDecoded(CSRs.frm) || csrDecoded(CSRs.fcsr)
    val csrAddrPrv = io.rw.addr(9, 8)
    val readOnly = io.rw.addr(11, 10).andR
    val prvMatch = regMstatus.prv >= csrAddrPrv
    val cpuWen = cpuRen && io.rw.cmd =/= CSR.R && prvMatch
    val wen = cpuWen && !readOnly
    val wdata = MuxLookup(io.rw.cmd, 0.U, Seq(
      CSR.W -> io.rw.wdata,
      CSR.C -> (io.rw.rdata & (~io.rw.wdata).asUInt),
      CSR.S -> (io.rw.rdata | io.rw.wdata)
    ))

    // exception and irq
    val instrCall = io.rw.addr === Instructions.SCALL(31, 12) && systemInstr
    val instrBreak = io.rw.addr === Instructions.SBREAK(31, 12) && systemInstr
    val instrRet = io.rw.addr === Instructions.SRET(31, 12) && systemInstr && prvMatch
    val instrSfenceVM = io.rw.addr === Instructions.SFENCE_VM && systemInstr && prvMatch
    val instrRedirect = io.rw.addr(2) && systemInstr
    val instrRedirectPrv = instrRedirect && prvMatch
    val instrWfi = io.rw.addr === Instructions.WFI(31, 12) && systemInstr && prvMatch

    val csrXcpt = (cpuWen && readOnly) ||
      (cpuRen && (!prvMatch || !addrValid || isFP && !io.status.fs.orR)) ||
      (systemInstr && !prvMatch) ||
      instrCall || instrBreak

    when (instrWfi) {
      regWFI := true.B
    } .elsewhen (interruptPending) {
      regWFI := false.B
    }

    io.evec := MuxCase(regSepc, Seq(
      (io.exception || csrXcpt) -> ((regMstatus.prv << 6).asUInt + MTVEC.U),
      instrRedirect -> regStvec.asTypeOf(UInt(vAddrBitsExtended.W)),
      regMstatus.prv(1) -> regMepc
    ))
    io.ptbr := regSptbr
    io.csrXcpt := csrXcpt
    io.eret := instrRet || instrRedirectPrv
    io.status := regMstatus
    io.status.fs := Fill(2, regMstatus.fs.orR)
    io.status.xs := Fill(2, regMstatus.xs.orR)
    io.status.sd := regMstatus.xs.orR || regMstatus.fs.orR
    if (xLen == 32) io.status.sdRv32 := io.status.sd

    when (io.exception || csrXcpt) {
      regMstatus.ie := false.B
      regMstatus.prv := PRV_M.asUInt
      regMstatus.mPrv := false.B
      regMstatus.prv1 := regMstatus.prv
      regMstatus.ie1 := regMstatus.ie
      regMstatus.prv2 := regMstatus.prv1
      regMstatus.ie2 := regMstatus.ie1

      regMepc := fromEPC(io.pc) // TODO: check
      regMcause := io.cause
      when (csrXcpt) {
        regMcause := Causes.illegal_instruction.U
        when (instrBreak) {
          regMcause := Causes.breakpoint.U
        }
        when (instrCall) {
          regMcause := regMstatus.prv + Causes.user_ecall.U
        }
      }

      regMbadaddr := io.pc
      when (io.cause === Causes.fault_load.U || io.cause === Causes.fault_store.U ||
        io.cause === Causes.misaligned_load.U || io.cause === Causes.misaligned_store.U) {
        val (upper, lower) = (io.rw.wdata(xLen - 1, vAddrBits), io.rw.wdata(vAddrBits - 1, 0))
        val sign = Mux(lower.asSInt < 0.S, upper.andR, upper.orR) // TODO: why?
        regMbadaddr := Cat(sign, lower)
      }
    }

    when (instrRet) {
      regMstatus.ie := regMstatus.ie1
      regMstatus.prv := regMstatus.prv1
      regMstatus.ie1 := regMstatus.ie2
      regMstatus.prv1 := regMstatus.prv2
      regMstatus.ie2 := true.B
      regMstatus.prv2 := PRV_U.asUInt
    }

    when (instrRedirectPrv) {
      regMstatus.prv := PRV_S.asUInt
      regSbadaddr := regMbadaddr
      regScause := regMcause
      regSepc := regMepc
    }

    regMip.stip := (regUtime.value(regStimecmp.getWidth - 1, 0) === regStimecmp)

    // csr rd
    io.rw.rdata := Mux1H(csrMap.map(reg => (csrDecoded(reg._1), reg._2)))
    io.time := regUtime.value
    io.csrStall := regWFI
    io.frm := regUfrm
    when (io.fflags.valid) {
      regUfflags := FPConstants.safe(regUfflags.asUInt | io.fflags.bits)._1
    }

    // csr wr
    when (wen) {
      switch (io.rw.addr) {
        is(CSRs.mstatus.U) {
          val mStatus = wdata.asTypeOf(new MStatus)
          val supportModes = VecInit((PRV_M :: PRV_U :: (if (p(UseVM)) List(PRV_S) else Nil)).map(_.asUInt))
          regMstatus.ie := mStatus.ie
          regMstatus.ie1 := mStatus.ie1
          regMstatus.mPrv := mStatus.mPrv
          when (supportModes contains mStatus.prv) { regMstatus.prv := mStatus.prv }
          when (supportModes contains mStatus.prv1) { regMstatus.prv1 := mStatus.prv1 }
          if (p(UseVM)) {
            regMstatus.ie2 := mStatus.ie2
            when (supportModes contains mStatus.prv2) { regMstatus.prv2 := mStatus.prv2 }

            val vm = if (xLen == 32) 8 else 9 // sv32 vs sv39
            when (mStatus.vm === 0.U || mStatus.vm === vm.U) { regMstatus.vm := mStatus.vm }
          }
          if (p(BuildFPU).nonEmpty) regMstatus.fs := mStatus.fs
          if (p(BuildRoCC).nonEmpty) regMstatus.xs := mStatus.xs
        }
        is (CSRs.mip.U) {
          val mip = wdata.asTypeOf(new MIP)
          regMip.msip := mip.msip
          if (p(UseVM)) regMip.ssip := mip.ssip
        }
        is (CSRs.mie.U) {
          val mie = wdata.asTypeOf(new MIP)
          regMie.msip := mie.msip
          regMie.mtip := mie.mtip
          if (p(UseVM)) {
            regMie.ssip := mie.ssip
            regMie.stip := mie.stip
          }
        }
        is (CSRs.fflags.U) {
          regUfflags := FPConstants.safe(wdata(regUfflags.getWidth - 1, 0))._1
        }
        is (CSRs.frm.U) { regUfrm := wdata }
        is (CSRs.fcsr.U) {
          regUfflags := FPConstants.safe(wdata(regUfflags.getWidth - 1, 0))._1
          regUfrm := wdata >> regUfflags.getWidth
        }
        is (CSRs.mepc.U) {
          regMepc := fromEPC(wdata(vAddrBitsExtended - 1, 0)) // TODO: check
        }
        is (CSRs.mscratch.U) { regMscratch := wdata }
        is (CSRs.mcause.U) {
          regMcause := wdata & (BigInt(1) << (xLen - 1) + 31).U
        }
        is (CSRs.mbadaddr.U) { regMbadaddr := wdata(vAddrBitsExtended - 1, 0) }
        is (CSRs.cyclew.U) { regUtime := wdata }
        is (CSRs.instretw.U) { regUinstret := wdata }
        is (CSRs.timew.U) { regUtime := wdata }
        is (CSRs.stimew.U) { regUtime := wdata }
        is (CSRs.stimecmp.U) {
          regStimecmp := wdata(31, 0)
          regMip.stip := false.B
        }
      }
      if (p(UseVM)) {
        switch (io.rw.addr) {
          is(CSRs.sstatus.U) {
            val sStatus = wdata.asTypeOf(new SStatus)
            regMstatus.ie := sStatus.ie
            regMstatus.ie1 := sStatus.pie
            regMstatus.mPrv := sStatus.mPrv
            regMstatus.prv1 := Mux(sStatus.ps, PRV_S, PRV_U).asUInt
            regMstatus.fs := sStatus.fs
            if (p(BuildRoCC).nonEmpty) regMstatus.xs := sStatus.xs
          }
          is(CSRs.sip.U) {
            val sip = wdata.asTypeOf(new MIP)
            regMip.ssip := sip.msip
          }
          is(CSRs.sie.U) {
            val sie = wdata.asTypeOf(new MIP)
            regMie.ssip := sie.ssip
            regMie.stip := sie.stip
          }
          is (CSRs.sscratch.U) { regSscratch := wdata }
          is (CSRs.sptbr.U) { regSptbr := Cat(wdata(pAddrBits - 1, pgIdxBits), 0.U(pgIdxBits.W)) }
          is (CSRs.sepc.U) { regSepc := fromEPC(wdata(vAddrBitsExtended - 1, 0)) }
          is (CSRs.stvec.U) { regStvec := fromEPC(wdata(vAddrBits - 1, 0)) }
        }
      }
    }
  }

  private def fromEPC(x: UInt): UInt = x & ((~coreInstBytes.U(vAddrBitsExtended.W)).asUInt + 1.U)
}

object CSRTest extends App {
  implicit val p = Parameters((site, here, up) => {
    case UseVM => true
    case CoreKey => new CoreParams {
      override def xLen: Int = 64
      override def pAddrBits: Int = 50
      override def vAddrBits: Int = 39
      override def pgIdxBits: Int = 12
      override def ppnBits: Int = 38
      override def vpnBits: Int = 27
      override def pgLevels: Int = 3
      override def asIdBits: Int = 0
      override def pgLevelBits: Int = 0
      override def retireWidth: Int = 0
      override def coreFetchWidth: Int = 0
      override def coreInstBits: Int = 32
      override def coreDCacheRegTagBits: Int = 0
      override def fastLoadByte: Boolean = false
      override def fastLoadWord: Boolean = false
      override def maxHartIdBits: Int = 0
    }
    case BuildRoCC => None
    case BuildFPU => None
  })
  generate(new CSR())
}
