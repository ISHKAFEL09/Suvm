package rocket2.agents.ptwslave

import chisel3._
import rocket2._

case class PTWSlaveIF(clk: Clock, tlb2ptw: TLB2PTWIO)
