package chiter

import chisel3._

case class ClockInfo(clk: Clock, period: BigInt) {
  var periodLeft: BigInt = period
}
