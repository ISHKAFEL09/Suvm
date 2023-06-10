package uvm.chiter

import chisel3._

import scala.collection.mutable.ListBuffer

trait ChiterHarness extends Module {
  val clocks: ListBuffer[ClockInfo] = ListBuffer.empty[ClockInfo]

  def clock(clk: Clock, period: BigInt, duty: Double = 0.5): Unit =
    clocks += ClockInfo(clk, period)
}
