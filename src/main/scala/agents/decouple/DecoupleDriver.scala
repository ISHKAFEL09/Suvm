package agents.decouple

import chisel3._
import chisel3.util._
import chiseltester._
import uvm._

abstract class DecoupleDriver[T, REQ <: Data](name: String,
                                  parent: Option[UVMComponent],
                                  val bus: DecoupledIO[REQ])(implicit clk: Clock)
  extends UVMDriver[DecoupleSeqItem[T], DecoupleSeqItem[T]](name, parent) {

  def driver(t: T): REQ

  override def runPhase(phase: UVMPhase): Unit = {
    bus.valid.poke(false.B)
    while (true) {
      val item = seqItemPort.getNextItem
      uvmInfo(getTypeName, s"get item: ${item.name}", UVM_NONE)
      bus.valid.poke(true.B)
      bus.bits.poke(driver(item.gen))
      clk.step()
      while (!bus.ready.peek().litToBoolean)
        clk.step()
      bus.valid.poke(false.B)
      seqItemPort.itemDone()
      uvmInfo(getTypeName, s"item: ${item.name} done", UVM_NONE)
    }
  }
}
