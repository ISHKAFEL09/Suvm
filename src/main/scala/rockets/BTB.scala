package rockets

import spinal.core._
import spinal.lib._

case class BtbIO() extends Bundle with IMasterSlave{
  val currentPC, target, correctPC, correctTarget = UInt(32 bits)
  val hit, wen = Bool()

  override def asMaster(): Unit = {
    in(currentPC, wen, correctPC, correctTarget)
    out(hit, target)
  }
}

case class BTB() extends Component {
  val io = master(BtbIO())

  val tagMem: Mem[Bool] = Mem(Bool(), 4)
  val dataMem: Mem[UInt] = Mem(UInt((28 + 30) bits), 4)

  val valid = tagMem(io.currentPC(3 downto 2))
  val target = dataMem(io.currentPC(3 downto 2))

  when (io.wen) {
    tagMem.write(io.correctPC(2 to 3), True)
    dataMem.write(io.correctPC(2 to 3), io.correctPC(4 to 31) @@ io.correctTarget(2 to 31))
  }

  io.hit := valid & (target(57 downto 30) === io.currentPC(31 downto 4))
  io.target := target(29 downto 0) @@ U"2'h0"
}

object BtbIO extends App {
  generate(BTB())
}
