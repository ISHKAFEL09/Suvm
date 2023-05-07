package rocket2

import chisel3._
import chisel3.util._
import config._

class RoCCIO extends Bundle {
  val interrupt = Output(Bool())
}

abstract class RoCC(implicit p: Parameters) extends CoreModule {

}
