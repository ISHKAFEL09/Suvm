package rocket2

import chisel3._
import chisel3.experimental.FlatIO

class Data2IMemIO extends Bundle {
  val reqAddr = Output(UInt(32.W))
  val respData = Input(UInt(32.W))
}

class DataPathIO extends Bundle {
  val ctrl = Flipped(new Ctrl2DataIO)
  val debug = new DebugIO
  val dMem = Flipped(new Core2DMemIO)
  val iMem = Flipped(new Core2IMemIO)
}

class DataPath extends Module {
  val io = FlatIO(new DataPathIO)

  // instruction fetch stage
  val btb = Module(new BTB)
  val csrRdPC = Wire(UInt(32.W))
  val branchPC = Wire(UInt(32.W))

}

object DataPath extends App {
  generate(new DataPath)
}
