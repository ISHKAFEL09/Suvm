package agents.decouple

import chisel3._
import uvm._

case class DecoupleSeqItem[T](override val name: String, gen: T) extends UVMSequenceItem(name)
