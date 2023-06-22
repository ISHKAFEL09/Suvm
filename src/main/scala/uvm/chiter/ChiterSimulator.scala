package uvm.chiter

import chisel3._
import chisel3.reflect.DataMirror
import firrtl.AnnotationSeq
import firrtl._
import firrtl.annotations.NoTargetAnnotation
import firrtl.transforms._

sealed trait FinishStatus
object SuccessStatus extends FinishStatus
object StopStatus extends FinishStatus
object FatalStatus extends FinishStatus
case class TestFinishedException(t: BigInt) extends Exception("Test Finished!")

sealed trait VerilatedOption extends NoTargetAnnotation
case class VerilatorFlags(flags: Seq[String]) extends VerilatedOption
case class VerilatorCFlags(flags: Seq[String]) extends VerilatedOption
case class VerilatorLinkFlags(flags: Seq[String]) extends VerilatedOption

case class TimeoutValue(value: BigInt) extends NoTargetAnnotation

private[uvm] case class TopModuleInfo(name: String,
                                      inputs: Seq[PinInfo],
                                      outputs: Seq[PinInfo],
                                      clocks: Seq[PinInfo]) {
  require(inputs.forall(_.width > 0), s"Inputs need to be at least 1-bit!\n$inputs")
  require(outputs.forall(_.width > 0), s"Outputs need to be at least 1-bit!\n$outputs")
  def portNames: Seq[String] = inputs.map(_.name) ++ outputs.map(_.name) ++ clocks.map(_.name)
  val pokeable: Seq[(PinInfo, Int)] = (inputs ++ clocks).zipWithIndex
  val peekable: Seq[(PinInfo, Int)] = (inputs ++ outputs ++ clocks).zipWithIndex
}

private[uvm] case class PinInfo(name: String, width: Int, signed: Boolean) {
  val mask: BigInt = (BigInt(1) << width) - 1
}

private[uvm] object TopModuleInfo {
  def apply(circuit: ir.Circuit): TopModuleInfo = {
    val main = circuit.modules.find(_.name == circuit.main).get

    def isClockIn(p: ir.Port): Boolean = p.tpe == ir.ClockType && p.direction == ir.Input
    val (clock, notClock) = main.ports.partition(isClockIn)
    val (in, out) = notClock.filterNot(p => bitWidth(p.tpe) == 0).partition(_.direction == ir.Input)

    new TopModuleInfo(
      name = main.name,
      inputs = in.map(portNameAndWidthAndIsSigned),
      outputs = out.map(portNameAndWidthAndIsSigned),
      clocks = clock.map(i => PinInfo(i.name, 1, signed = false))
    )
  }

  private def portNameAndWidthAndIsSigned(p: ir.Port): PinInfo = {
    require(
      p.tpe.isInstanceOf[ir.GroundType],
      s"Port ${p.serialize} is not of ground type! Please make sure to provide LowFirrtl to this API!"
    )
    PinInfo(p.name, bitWidth(p.tpe).toInt, p.tpe.isInstanceOf[ir.SIntType])
  }
}

trait ChiterSimulator {
  var dut: Option[ChiterHarness] = None
  val ports = collection.mutable.Map.empty[Data, String]
  val paths = collection.mutable.Map.empty[Data, Set[Data]]
  var timeout: BigInt = 0

  def getTimeNow: BigInt

  def poke(signal: Data, value: BigInt): Unit

  def peek(signal: Data): BigInt

  def step(n: Int): Unit

  def update(): Unit = {}

  def simFinish(): Unit

  def resolveName(signal: Data): String = {
    ports.getOrElse(signal, signal.toString)
  }

  private def getDataNames(name: String, data: Data): Seq[(Data, String)] = Seq(data -> name) ++ (data match {
    case _: Element => Seq()
    case r: Record => r.elements.flatMap { case (n, e) => getDataNames(s"${name}_$n", e) }
    case v: Vec[_] => v.zipWithIndex.flatMap { case (e, i) => getDataNames(s"${name}_$i", e) }
  })

  private def combPathsToData(dut: Module,
                              paths: Seq[CombinationalPath],
                              names: Map[Data, String]): Map[Data, Set[Data]] = {
    val nameToData = names.map { case (port, name) => name -> port }
    paths.filter { p =>
      p.sink.module == dut.name && p.sources.exists(_.module == dut.name)
    }.map { p =>
      p.sink.name -> p.sources.filter(_.module == dut.name).map(_.name)
    }.map { case (sink, sources) =>
      nameToData(sink) -> sources.map(nameToData).toSet
    }.toMap
  }

  def createTester(rtl: CircuitState, needCompile: Boolean): Unit

  def build[T <: ChiterHarness](dutGen: () => T, annotations: AnnotationSeq, compile: Boolean = true): Unit = {
    // elaborate the design
    val (highFirrtl, d) = Compiler.elaborate(dutGen, annotations)
    dut = Some(d)

    timeout = highFirrtl.annotations.collectFirst { case TimeoutValue(t) => t }.getOrElse(0)

    val lowFirrtl = Compiler.toLowFirrtl(highFirrtl)
    ports ++= DataMirror.modulePorts(d).flatMap { case (name, data) =>
      getDataNames(name, data).toList
    }.toMap


    val pathAnnotations = (new CheckCombLoops).execute(lowFirrtl).annotations
    val path = pathAnnotations.collect { case c: CombinationalPath => c }
    paths ++= combPathsToData(d, path, ports.toMap)

    createTester(lowFirrtl, compile)
  }
}
