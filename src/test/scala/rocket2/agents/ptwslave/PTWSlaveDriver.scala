package rocket2.agents.ptwslave

import chisel3._
import uvm._

import scala.util.Random

class PTWSlaveDriver (name: String, parent: UVMComponent, val bus: PTWSlaveIF)
  extends UVMDriver[PTWSlaveItem, PTWSlaveItem](name, Some(parent)) {

  private def vpn2ppn(vpn: BigInt): BigInt = vpn

  private def sendRsp(item: PTWSlaveItem): Unit = {
    bus.tlb2ptw.resp.valid.poke(true.B)
    bus.tlb2ptw.resp.bits.error.poke(false.B)
    bus.tlb2ptw.resp.bits.pte.valid.poke(item.pte.valid.B)
    bus.tlb2ptw.resp.bits.pte.ppn.poke(item.pte.ppn.U)
    bus.tlb2ptw.resp.bits.pte.dirty.poke(item.pte.dirty.B)
    bus.tlb2ptw.resp.bits.pte.refer.poke(item.pte.refer.B)
    bus.tlb2ptw.resp.bits.pte.typ.poke(item.pte.typ.U)
  }

  override def runPhase(phase: UVMPhase): Unit = {
    val req = bus.tlb2ptw.req
    val resp = bus.tlb2ptw.resp
    val clk = bus.clk

    while (true) {
      req.ready.poke(true.B)
      resp.valid.poke(false.B)
      clk.step()
      while (req.valid.peek().litToBoolean)
        clk.step()

      req.ready.poke(false.B)
      val vpn = req.bits.addr.peek().litValue
      val pte = PTEItem(
        ppn = vpn2ppn(vpn),
        dirty = Random.nextBoolean(),
        refer = Random.nextBoolean(),
        typ = 7,
        valid = true)
      val item = PTWSlaveItem(vpn, pte)
      sendRsp(item)
      clk.step()
    }
  }
}
