package chiseltester

import chisel3._
import chisel3.experimental._
import chisel3.internal.firrtl.Circuit
import chisel3.reflect.DataMirror
import chisel3.stage._
import chisel3.stage.phases.{Convert, Elaborate, MaybeAspectPhase}
import circt.stage.ChiselStage
import firrtl._
import firrtl.stage._
import firrtl.annotations._
import firrtl.transforms.{CheckCombLoops, CombinationalPath}
import logger._
import treadle.stage.TreadleTesterPhase
import treadle.{TreadleCircuitStateAnnotation, TreadleTesterAnnotation}


object TreadleExecutive {
  def getTopModule(circuit: Circuit): BaseModule = {
    circuit.components.find(_.name == circuit.name).get.id
  }

  def getDataNames(name: String, data: Data): Seq[(Data, String)] = Seq(data -> name) ++ (data match {
    case _: Element => Seq()
    case r: Record => r.elements.flatMap { case (n, e) => getDataNames(s"${name}_$n", e) }
    case v: Vec[_] => v.zipWithIndex.flatMap { case (e, i) => getDataNames(s"${name}_$i", e) }
  })

  def combPathsToData(dut: BaseModule,
                      paths: Seq[CombinationalPath],
                      names: Map[Data, String]): Map[Data, Set[Data]] = {
    val nameToData = names.map { case(port, name) => name -> port }
    paths.filter { p =>
      p.sink.module == dut.name && p.sources.exists(_.module == dut.name)
    } .map { p =>
      p.sink.name -> p.sources.filter(_.module == dut.name).map(_.name)
    } .map { case(sink, sources) =>
      nameToData(sink) -> sources.map(nameToData).toSet
    }.toMap
  }

  def start[T <: Module](dutGen: () => T,
                         annotationSeq: AnnotationSeq): BackendInstance[T] = {
    // elaborate the design
    val (highFirrtl, dut, annoSeq) = Compiler.elaborate(dutGen, annotationSeq)
    val lowFirrtl = Compiler.toLowFirrtl(highFirrtl)
    val portNames = DataMirror.modulePorts(dut).flatMap { case (name, data) =>
      getDataNames(name, data).toList
    }.toMap
    val pathAnnotations = (new CheckCombLoops).execute(lowFirrtl).annotations
    val paths = pathAnnotations.collect { case c: CombinationalPath => c }
    val pathsAsData = combPathsToData(dut, paths, portNames)
    val treadleAnnotations = (new TreadleTesterPhase).transform(annoSeq)
    val treadleTester = treadleAnnotations.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(s"TreadleTesterPhase did not build a treadle tester")
    )
    new TreadleBackend[T](dut, portNames, pathsAsData, treadleTester)
  }
}
