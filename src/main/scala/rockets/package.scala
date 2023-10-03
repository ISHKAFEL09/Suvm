import spinal.core._
import spinal.lib._

package object rockets {
  def generate(design: => Component): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "spinal_gen"
    ).generate(design)
  }
}
