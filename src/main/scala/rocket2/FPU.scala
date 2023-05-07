package rocket2

import chisel3._
import chisel3.util._

object FPConstants extends ChiselEnum
{
  val FCMD_ADD = Value
  val FCMD_SUB = Value
  val FCMD_MUL = Value
  val FCMD_MADD = Value
  val FCMD_MSUB = Value
  val FCMD_NMSUB =  Value
  val FCMD_NMADD =  Value
  val FCMD_DIV = Value
  val FCMD_SQRT = Value
  val FCMD_SGNJ = Value
  val FCMD_MINMAX = Value
  val FCMD_CVT_FF = Value
  val FCMD_CVT_IF = Value
  val FCMD_CMP = Value
  val FCMD_MV_XF =  Value
  val FCMD_CVT_FI = Value
  val FCMD_MV_FX =  Value
  val FCMD_X = Value("h1F".U)
  val RM_SZ = 3
}

class FPU {

}
