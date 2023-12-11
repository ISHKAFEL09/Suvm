import spinal.core._
import spinal.lib._

package object rockets {
  def generate(design: => Component): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "spinal_gen"
    ).generate(design).printPruned()
  }

  implicit class EnumElementOps[T <: SpinalEnum](e: SpinalEnumElement[T]) {
    def asUInt: UInt = e.asBits.asUInt
  }

  implicit class EnumCraftOps[T <: SpinalEnum](e: SpinalEnumCraft[T]) {
    def asUInt: UInt = e.asBits.asUInt
  }
}
