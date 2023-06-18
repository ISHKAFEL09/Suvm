package rocket2.agents.tlbreq

import agents.decouple._
import chisel3._
import chisel3.experimental.BundleLiterals._
import rocket2.TLBReq
import uvm._

class TLBReqDriver(name: String, parent: UVMComponent, bus: TLBReqIF)
  extends DecoupleDriver[TLBReqItem, TLBReq](name, Some(parent), bus.clk, bus) {

  override def driver(t: TLBReqItem): TLBReq = chiselTypeOf(bus.tlbReq.bits).Lit(
    _.vpn -> t.vpn.U,
    _.asid -> t.asid.U,
    _.passthrough -> t.passthrough.B,
    _.store -> t.store.B,
    _.instruction -> t.instruction.B
  )
}
