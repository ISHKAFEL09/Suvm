package uvm.chiter

import chisel3._

import scala.util.Random

case class ClockInfo(clk: Clock, period: BigInt) {
  var periodLeft: BigInt = Random.between(1, period.toInt)
}
