package rocket2

import chisel3._
import chisel3.experimental.FlatIO
import chisel3.util._

class BtbIO extends Bundle {
  val currentPC = Input(UInt(32.W))
  val hit = Output(Bool())
  val target = Output(UInt(32.W))
  val wen = Input(Bool())
  val correctPC = Input(UInt(32.W))
  val correctTarget = Input(UInt(32.W))
}

class BTB extends Module {
  val io = FlatIO(new BtbIO)

  val tagMem = Mem(4, Bool())
  val dataMem = Mem(4, UInt((28 + 30).W))

  val valid = tagMem.read(io.currentPC(3, 2))
  val target = dataMem.read(io.currentPC(3, 2))

  when (io.wen) {
    tagMem.write(io.correctPC(3, 2), true.B)
    dataMem.write(io.correctPC(3, 2), Cat(io.correctPC(31, 4), io.correctTarget(31, 2)))
  }

  io.hit := valid & (target(57, 30) === io.currentPC(31, 4))
  io.target := Cat(target(29, 0), 0.U(2.W))
}

object BtbIO extends App {
  generate(new BTB)
}
