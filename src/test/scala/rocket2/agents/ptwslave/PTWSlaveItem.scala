package rocket2.agents.ptwslave

import uvm._

case class PTEItem(ppn: BigInt, dirty: Boolean, refer: Boolean, typ: Int, valid: Boolean)

case class PTWSlaveItem(vpn: BigInt, pte: PTEItem) extends UVMSequenceItem("ptwSlaveItem")
