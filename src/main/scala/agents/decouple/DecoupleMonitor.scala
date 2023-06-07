package agents.decouple

import chiseltester._
import chisel3._
import chisel3.util._
import uvm._

abstract class DecoupleMonitor[REQ, T <: Data](name: String,
                                            parent: Option[UVMComponent],
                                            val bus: DecoupledIO[T])(implicit clk: Clock)
  extends UVMMonitor(name, parent) {

  val ap = new UVMAnalysisImp[DecoupleSeqItem[REQ]]("ap", write)

  def monitor(): DecoupleSeqItem[REQ]

  def write(t: DecoupleSeqItem[REQ]): Unit

  override def runPhase(phase: UVMPhase): Unit = {
    while (true) {
      clk.step()
      while (!bus.ready.peek().litToBoolean || !bus.valid.peek().litToBoolean)
        clk.step()
      ap.write(monitor())
    }
  }
}
