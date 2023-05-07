package rocket2

import chisel3._
import chisel3.util._

class Core2DMemReq extends Bundle {
  val cmd = UInt(4.W)
  val tp = UInt(3.W)
  val addr = UInt(32.W)
  val data = UInt(64.W)
  val tag = UInt(12.W)
}

class DMem2CoreResp extends Bundle {
  val tag = UInt(12.W)
  val data = UInt(64.W)
}

class Core2DMemIO extends Bundle {
  val req = Flipped(DecoupledIO(new Core2DMemReq))
  val resp = ValidIO(new DMem2CoreResp)
}

class DCache {

}
