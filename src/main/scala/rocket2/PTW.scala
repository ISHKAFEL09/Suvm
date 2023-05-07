package rocket2

import chisel3._
import chisel3.util._
import rocket2.config._

class PTE(implicit p: Parameters) extends CoreBundle {
  val ppn = UInt(ppnBits.W)
  val rsv = UInt(3.W)
  val dirty = Bool()
  val refer = Bool()
  val typ = UInt(4.W)
  val valid = Bool()

  def isTable = valid && typ < 2.U
  def isLeaf = valid && typ >= 2.U
  def uRead = isLeaf && typ < 8.U
  def uWrite = uRead && typ(0)
  def uExecute= uRead && typ(1)
  def sRead = isLeaf
  def sWrite = isLeaf && typ(0)
  def sExecute = valid && typ >= 4.U && typ(1)
  def accessValid(prv: UInt, store: Bool, fetch: Bool): Bool =
    Mux(prv(0),
      Mux(fetch, sExecute, Mux(store, sWrite, sRead)),
      Mux(fetch, uExecute, Mux(store, uWrite, uRead)))
}

class PTWReq(implicit p: Parameters) extends CoreBundle {
  val addr = UInt(vpnBits.W)
  val prv = UInt(2.W)
  val store = Bool()
  val fetch = Bool()
}

class PTWResp(implicit p: Parameters) extends CoreBundle {
  val error = Bool()
  val pte = new PTE
}

class TLB2PTWIO(implicit p: Parameters) extends CoreBundle {
  val req = DecoupledIO(new PTWReq)
  val resp = Flipped(ValidIO(new PTWResp))
  val status = Input(new MStatus)
  val invalidate = Input(Bool())
}

class PTW {

}
