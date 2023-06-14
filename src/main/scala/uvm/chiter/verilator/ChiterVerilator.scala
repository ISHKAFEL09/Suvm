package uvm.chiter.verilator

import com.sun.jna._
import uvm.chiter._
import org.fusesource.scalate._

trait ChiterVerilator extends ChiterSimulator {
  var so: Option[NativeLibrary] = None
  var ptr: Option[Pointer] = None

//  override def getTimeNow: BigInt = ???
//
//  override def poke(signal: Data, value: BigInt): Unit = ???
//
//  override def peek(signal: Data): BigInt = ???
//
//  override def step(n: Int): Unit = ???
//
//  override def createTester(rtl: CircuitState, annoSeq: AnnotationSeq): Unit = ???
}

object ChiterVerilator {
  def run(): Unit = {
    val target = os.Path("/home/wang/Projects/Suvm/test_run_dir") / "Harness.cpp"
    val template = getClass.getClassLoader.getResource("uvm/chiter/verilator/VerilatorHarness.mustache").getPath
    val engine = new TemplateEngine
    os.remove(target)
    val cpp = engine.layout(new java.io.File(template).getCanonicalPath, Map(
      "dutVerilatorClassName" -> "top"
    ))
    os.write(target, cpp)
  }
}