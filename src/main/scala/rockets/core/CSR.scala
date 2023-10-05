package rockets.core

import spinal.core._

/** CSR-MStatus register, refer to priv-spec */
case class MStatus() extends Bundle {
  val sd = Bool()
  val zero2 = UInt(31 bits)
  val sd_rv32 = UInt(1 bits)
  val zero1 = UInt(9 bits)
  val vm = UInt(5 bits)
  val mprv = Bool()
  val xs = UInt(2 bits)
  val fs = UInt(2 bits)
  val prv3 = UInt(2 bits)
  val ie3 = Bool()
  val prv2 = UInt(2 bits)
  val ie2 = Bool()
  val prv1 = PrivEnum()
  val ie1 = Bool()
  val prv = PrivEnum()
  val ie = Bool()
}
