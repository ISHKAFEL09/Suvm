package agents.decouple

import chisel3._
import chisel3.util._
import uvm._

abstract class DecoupleMonitor[REQ <: UVMSequenceItem, T <: Data](name: String,
                                                                  parent: Option[UVMComponent],
                                                                  clk: Clock,
                                                                  val bus: DecoupleIF[T])
  extends UVMMonitor(name, parent) {

  val ap = new UVMAnalysisImp[REQ]("ap", write)

  def monitor(): REQ

  def write(t: REQ): Unit

  override def runPhase(phase: UVMPhase): Unit = {
    while (true) {
      clk.step(posedge = false)
      while (!bus.ready.peek().litToBoolean || !bus.valid.peek().litToBoolean)
        clk.step(posedge = false)
      ap.write(monitor())
    }
  }
}
