package agents.decouple

import chisel3._

trait DecoupleIF[T] {
  def clk: Clock

  def valid: Bool

  def ready: Bool

  def bits: T
}
