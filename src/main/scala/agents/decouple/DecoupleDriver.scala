package agents.decouple

import chisel3._
import chisel3.util._
import uvm._

abstract class DecoupleDriver[T <: UVMSequenceItem, REQ <: Data](name: String,
                                              parent: Option[UVMComponent],
                                              clk: Clock,
                                              val bus: DecoupleIF[REQ])
  extends UVMDriver[T, T](name, parent) {

  def driver(t: T): REQ

  override def runPhase(phase: UVMPhase): Unit = {
    bus.valid.poke(false.B)
    while (true) {
      val item = seqItemPort.getNextItem
      bus.valid.poke(true.B)
      bus.bits.poke(driver(item))
      clk.step()
      while (!bus.ready.peek().litToBoolean)
        clk.step()
      bus.valid.poke(false.B)
      seqItemPort.itemDone()
    }
  }
}
