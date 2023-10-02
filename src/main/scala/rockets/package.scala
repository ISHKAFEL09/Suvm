import spinal.core._
import spinal.lib._

package object rockets {
  def priorityEncoder(x: UInt): UInt =
    OHToUInt(x & (~x + 1))

  def generate(design: => Component): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "spinal_gen"
    ).generate(design)
  }
}
