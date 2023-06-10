package uvm.chiter

import chisel3._
import firrtl.AnnotationSeq

trait ChiterSimulator {
  def dut: Option[ChiterHarness]

  def getTimeNow: BigInt

  def poke(signal: Data, value: BigInt): Unit

  def peek(signal: Data): BigInt

  def step(n: Int): Unit

  def build[T <: ChiterHarness](dutGen: () => T, annotations: AnnotationSeq): Unit
}
