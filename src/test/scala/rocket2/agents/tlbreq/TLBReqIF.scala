package rocket2.agents.tlbreq

import chisel3._
import chisel3.util._
import rocket2._
import _root_.agents.decouple.DecoupleIF

case class TLBReqIF(clk: Clock, tlbReq: DecoupledIO[TLBReq]) extends DecoupleIF[TLBReq] {
  override def valid: Bool = tlbReq.valid

  override def ready: Bool = tlbReq.ready

  override def bits: TLBReq = tlbReq.bits
}
