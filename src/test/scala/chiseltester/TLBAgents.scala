package chiseltester

import chisel3._
import chisel3.util._
import rocket2._
import rocket2.config._

class TLBReqAgent(req: DecoupledIO[TLBReq], clock: Clock) {

  def drive(vpn: UInt): Unit = {
    req.valid.poke(true.B)
    req.bits.vpn.poke(vpn)
    clock.step()
    req.valid.poke(false.B)
  }
}