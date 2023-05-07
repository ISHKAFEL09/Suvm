package rocket2

import chisel3._

trait Decoding {
  def uncorrected: Bits
  def corrected: Bits
  def correctable: Bool
  def uncorrectable: Bool
  def error: Bool = correctable | uncorrectable
}

trait Code {
  def width(w: Int): Int
  def encode(x: Bits): Bits
  def decode(x: Bits): Decoding
}

class IdentityCode extends Code {
  override def width(w: Int): Int = w

  override def encode(x: Bits): Bits = x

  override def decode(x: Bits): Decoding = new Decoding {
    override def uncorrected: Bits = x

    override def corrected: Bits = x

    override def correctable: Bool = false.B

    override def uncorrectable: Bool = false.B
  }
}
