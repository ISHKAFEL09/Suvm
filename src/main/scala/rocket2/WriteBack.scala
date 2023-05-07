package rocket2

import chisel3._

class WB2DataIO extends Bundle {
  val waddr = Output(UInt(5.W))
  val wen = Output(Bool())
  val wdata = Output(UInt(64.W))
}

class WriteBack {

}
