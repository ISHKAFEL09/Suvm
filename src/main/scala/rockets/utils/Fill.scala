package rockets.utils

import spinal.core._
import spinal.lib._

object Fill {
  def apply(n: Int, in: Bool): Bits = Vec.fill(n)(in).asBits
}

object FillInterleaved {
  def apply(n: Int, in: Bits): Bits = Cat(in.asBools.map(Fill(n, _)))
}
