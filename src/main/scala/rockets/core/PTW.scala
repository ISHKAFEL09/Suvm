package rockets.core

import rockets.generate
import rockets.params.HasDCacheParams
import rockets.params.config.Parameters
import spinal.core._
import spinal.lib._

abstract class PTWBundle(implicit val p: Parameters)
    extends Bundle
    with HasDCacheParams

abstract class PTWComponent(implicit val p: Parameters)
    extends Component
    with HasDCacheParams

/** pte entry, refer to priv-spec
  */
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

/**
 * request to ptw
 */
case class PTWReq()(implicit p: Parameters) extends PTWBundle {
  /** vpn address */
  val addr: UInt = UInt(vpnBits bits)
  /** request privilege */
  val prv: PrivEnum.C = PrivEnum()
  /** request from a store instr, needs write permission */
  val store = Bool()
  /** instruction fetch, needs execute permission */
  val fetch = Bool()
}

/**
 * pte response
 */
case class PTWResp()(implicit p: Parameters) extends PTWBundle {
  /** correspond pte entry */
  val pte: PTE = PTE()
  /** whether error happened */
  val error: Bool = Bool()
}

case class TLB2PTWIO()(implicit p: Parameters)
    extends PTWBundle
    with IMasterSlave {
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

case class PTW(reqNum: Int)(implicit p: Parameters) extends PTWComponent {
  val io = new Bundle {
    val requestor = Vec(slave(TLB2PTWIO()), reqNum)
  }
}

object PTWApp extends App {
  generate(PTW(4)(TLBApp.config))
}
