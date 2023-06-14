package uvm.chiter

import chisel3._
import chisel3.reflect.DataMirror
import firrtl.AnnotationSeq
import firrtl._
import firrtl.transforms._

sealed trait FinishStatus
object SuccessStatus extends FinishStatus
object StopStatus extends FinishStatus
object FatalStatus extends FinishStatus
object TestFinishedException extends Exception("Test Finished!")

trait ChiterSimulator {
  var dut: Option[ChiterHarness] = None
  val ports = collection.mutable.Map.empty[Data, String]
  val paths = collection.mutable.Map.empty[Data, Set[Data]]

  def getTimeNow: BigInt

  def poke(signal: Data, value: BigInt): Unit

  def peek(signal: Data): BigInt

  def step(n: Int): Unit

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

  def createTester(rtl: CircuitState, annoSeq: AnnotationSeq): Unit

  def build[T <: ChiterHarness](dutGen: () => T, annotations: AnnotationSeq): Unit = {
    // elaborate the design
    val (highFirrtl, d, annoSeq) = Compiler.elaborate(dutGen, annotations)
    dut = Some(d)

    val lowFirrtl = Compiler.toLowFirrtl(highFirrtl)
    ports ++= DataMirror.modulePorts(d).flatMap { case (name, data) =>
      getDataNames(name, data).toList
    }.toMap


    val pathAnnotations = (new CheckCombLoops).execute(lowFirrtl).annotations
    val path = pathAnnotations.collect { case c: CombinationalPath => c }
    paths ++= combPathsToData(d, path, ports.toMap)

    createTester(lowFirrtl, annoSeq)
  }
}
