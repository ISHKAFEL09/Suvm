import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.BitPat

import scala.language.implicitConversions

package object rocket2 {
  implicit class SeqOps[T <: Data](s: Seq[T]) {
    def contain(t: T): Bool = s.map(_.asUInt === t.asUInt).reduce(_ | _)
  }

  implicit class UIntOps(d: UInt) {
    def inside[S <: BitPat](s: Seq[S]): Bool = s.map(d.asUInt === _).reduce(_ || _)
  }

  implicit class BitPatOps(d: BitPat) {
    def asUInt: UInt = BitPat.bitPatToUInt(d)
  }

  implicit def BitPat2UInt(b: BitPat): UInt = BitPat.bitPatToUInt(b)

  def generate(gen: => Module): Unit = {
    (new ChiselStage).emitVerilog(gen, Array("--target-dir", "generated/rocket2"))
  }

  val MTVEC = 0x100
  val START_ADDR = MTVEC + 0x100
  val HART_ID = 0
}
