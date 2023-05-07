package rocket2

import chisel3._
import chisel3.util._
import config._
import tilelink.MemoryOpConstants._

class StoreGen(typ: UInt, addr: UInt, dat: UInt) {
  val isByte = typ === MT_B || typ === MT_BU
  val isHalf = typ === MT_H || typ === MT_HU
  val isWord = typ === MT_W || typ === MT_WU

  def mask: UInt = MuxCase("h255".U, Seq(
    isByte -> (1.U << addr(2, 0)).asUInt,
    isHalf -> (3.U << Cat(addr(2, 1), 0.U(1.W))).asUInt,
    isWord -> (15.U << Cat(addr(2), 0.U(2.W))).asUInt,
  ))

  lazy val wordData = Mux(isWord, Fill(2, dat(31, 0)), dat)

  def data: UInt = MuxCase(wordData, Seq(
    isByte -> Fill(8, dat(7, 0)),
    isHalf -> Fill(4, dat(15, 0)),
  ))
}

class LoadGen(typ: UInt, addr: UInt, dat: UInt, zero: Bool) {
  val sg = new StoreGen(typ, addr, dat)
  val hasSign = typ inside Seq(MT_B, MT_H, MT_W, MT_D)

  val wordShift = Mux(addr(2), dat(63, 32), dat(31, 0))
  val word = Cat(Mux(sg.isWord, Fill(32, hasSign && wordShift(31)), dat(63, 32)), wordShift)
  val halfShift = Mux(addr(1), word(31, 16), word(15, 0))
  val half = Cat(Mux(sg.isHalf, Fill(48, hasSign && halfShift(15)), word(63, 16)), halfShift)
  val byteShift = Mux(addr(0), half(15, 8), half(7, 0))
  val byte = Mux(zero, 0.U, Cat(Mux(sg.isByte, Fill(56, hasSign && byteShift(7)), half(63, 8)), byteShift))
}

class AMOALU(implicit p: Parameters) extends L1DCacheModule {
  val operandBits = p(AmoAluOperandBits)
  require(operandBits == 64)

  val io = IO(new Bundle {
    val addr = Input(UInt(blockOffBits.W))
    val cmd = Input(UInt(M_SZ.W))
    val typ = Input(UInt(MT_SZ.W))
    val lhs = Input(UInt(operandBits.W))
    val rhs = Input(UInt(operandBits.W))
    val out = Output(UInt(operandBits.W))
  })

  val storeGen = new StoreGen(io.typ, io.addr, io.rhs)
  val rhs = storeGen.wordData

  val cmdHasSign = io.cmd inside Seq(M_XA_MIN, M_XA_MAX)
  val cmdMax = io.cmd inside Seq(M_XA_MAX, M_XA_MAXU)
  val cmdMin = io.cmd inside Seq(M_XA_MIN, M_XA_MINU)
  val isWord = io.typ inside Seq(MT_W, MT_WU) //, MT_B, MT_BU)

  val addMask = -1.S(64.W).asUInt ^ (io.addr(2) << 31).asUInt
  val adderOut = (io.lhs & addMask) + (rhs & addMask)

  val lhsSign = Mux(isWord && !io.addr(2), io.lhs(31), io.lhs(63))
  val rhsSign = Mux(isWord && !io.addr(2), rhs(31), rhs(63))
  val ltl = io.lhs(31, 0) < rhs(31, 0)
  val ltu = io.lhs(63, 32) < rhs(63, 32)
  val equ = io.lhs(63, 32) === rhs(63, 32)
  val lt = Mux(isWord, Mux(io.addr(2), ltu, ltl), ltu || (equ && ltl))
  val less = Mux(lhsSign === rhsSign, lt, Mux(cmdHasSign, lhsSign, rhsSign))

  val out = MuxCase(storeGen.data, Seq(
    (io.cmd === M_XA_ADD) -> adderOut,
    (io.cmd === M_XA_AND) -> (io.lhs & rhs),
    (io.cmd === M_XA_OR) -> (io.lhs | rhs),
    (io.cmd === M_XA_XOR) -> (io.lhs ^ rhs),
    Mux(less, cmdMin, cmdMax) -> io.lhs
  ))
  val mask = FillInterleaved(8, storeGen.mask)
  io.out := (mask & out) | ((~mask).asUInt & io.lhs)
}
