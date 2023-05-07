package rocket2

import chisel3._

object PRV extends ChiselEnum {
  val PRV_U = Value(0.U)
  val PRV_S = Value(1.U)
  val PRV_H = Value(2.U)
  val PRV_M = Value(3.U)
}
