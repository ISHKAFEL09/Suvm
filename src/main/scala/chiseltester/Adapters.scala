package chiseltester

import chisel3._
import chisel3.util._

class ReadyValidSource[T <: Data](x: ReadyValidIO[T], clk: Clock) {
  def enqueue(data: T): Unit = {
    x.valid.poke(true.B)
    x.bits.poke(data)
    clk.step()
    while (!x.ready.peek().litToBoolean) {
      clk.step()
    }
    x.valid.poke(false.B)
  }
}

class ReadyValidSink[T <: Data](x: ReadyValidIO[T], clk: Clock) {
  def dequeue(): T = {
    x.ready.poke(true.B)
    clk.step()
    while (!x.valid.peek().litToBoolean) {
      clk.step()
    }
    x.bits.peek()
  }
}