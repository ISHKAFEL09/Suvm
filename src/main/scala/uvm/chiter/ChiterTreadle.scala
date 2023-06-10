package uvm.chiter

import chisel3._
import chisel3.reflect.DataMirror
import firrtl._
import firrtl.transforms._
import treadle._
import treadle.stage._
import uvm._

trait TreadleSimulator extends ChiterSimulator {
  var timeNow: BigInt = 0

  var dut: Option[ChiterHarness] = None
  val ports = collection.mutable.Map.empty[Data, String]
  val paths = collection.mutable.Map.empty[Data, Set[Data]]
  var tester: Option[TreadleTester] = None

  def getTimeNow: BigInt = timeNow

  def poke(signal: Data, value: BigInt): Unit = {
    tester.get.poke(ports(signal), value)
    debugLog(s"${resolveName(signal)} <- $value")
  }

  def peek(signal: Data): BigInt = {
    val value = tester.get.peek(ports(signal))
    debugLog(s"${resolveName(signal)} -> $value")
    value
  }

  def step(n: Int): Unit = {
    timeNow += 1
    tester.get.step(1)
  }

  private def resolveName(signal: Data): String = {
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

  override def build[T <: ChiterHarness](dutGen: () => T, annotations: AnnotationSeq): Unit = {
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

    val treadleAnnotations = (new TreadleTesterPhase).transform(annoSeq)
    tester = Some(treadleAnnotations.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(s"TreadleTesterPhase did not build a treadle tester")
    ))
  }
}

trait TreadleBackend extends ChiterBackend with ChiterMultiThreadBackend with TreadleSimulator
