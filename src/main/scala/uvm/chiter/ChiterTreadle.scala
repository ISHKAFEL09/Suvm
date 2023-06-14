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

  override def simFinish(): Unit = tester.get.finish

  override def createTester(rtl: CircuitState, annoSeq: AnnotationSeq): Unit = {
    val treadleAnnotations = (new TreadleTesterPhase).transform(annoSeq)
    tester = Some(treadleAnnotations.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(s"TreadleTesterPhase did not build a treadle tester")
    ))
  }
}

trait TreadleBackend extends ChiterBackend with ChiterMultiThreadBackend with TreadleSimulator
