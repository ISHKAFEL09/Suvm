package rockets

import params._
import params.config._
import spinal.core._
import spinal.lib._

abstract class PTWBundle(implicit val p: Parameters) extends Bundle with HasDCacheParams

case class PTE()(implicit p: Parameters) extends PTWBundle {
  val ppn: UInt = UInt(ppnBits bits)
  val dirty, ref, valid = Bool()
  val typ: UInt = UInt(4 bits)

  def table(dummy: Int = 0): Bool = valid && typ < 2

  def leaf(dummy: Int = 0): Bool = valid && typ >= 2

  def ur(dummy: Int = 0): Bool = leaf() && typ < 8

  def uw(dummy: Int = 0): Bool = ur() && typ(0)

  def ux(dummy: Int = 0): Bool = ur() && typ(1)

  def sr(dummy: Int = 0): Bool = leaf()

  def sw(dummy: Int = 0): Bool = leaf() && typ(0)

  def sx(dummy: Int = 0): Bool = valid && typ >= 4 && typ(1)
}

case class PTWReq()(implicit p: Parameters) extends PTWBundle {
  val addr: UInt = UInt(vpnBits bits)
  val prv: PrivEnum.C = PrivEnum()
  val store, fetch = Bool()
}

case class PTWResp()(implicit p: Parameters) extends PTWBundle {
  val pte: PTE = PTE()
  val error: Bool = Bool()
}

case class TLB2PTWIO()(implicit p: Parameters) extends PTWBundle with IMasterSlave {
  val req: Stream[PTWReq] = Stream(PTWReq())
  val resp: Flow[PTWResp] = Flow(PTWResp())
  val status: MStatus = MStatus()
  val invalidate: Bool = Bool()

  override def asMaster(): Unit = {
    master(req)
    slave(resp)
    in(status, invalidate)
  }
}