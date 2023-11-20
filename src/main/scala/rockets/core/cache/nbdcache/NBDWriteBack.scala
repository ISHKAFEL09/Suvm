package rockets.core.cache.nbdcache

import rockets.params._
import rockets.params.config._
import rockets.tilelink._
import spinal.core._
import spinal.lib._

case class NBDWriteBackReq()(implicit p: Parameters)
    extends Release
    with HasDCacheParams {
  val way = UInt(nWays bits)
}

case class NBDWriteBackIO()(implicit p: Parameters)
    extends NBDBundle
    with IMasterSlave {
  val req = Stream(NBDWriteBackReq())
  val metaReq = Stream(NBDMetaReadReq())
  val dataReq = Stream(NBDDataReadReq())
  val dataResp = UInt(encRowBits bits)
  val release = Stream(new Release())

  override def asMaster(): Unit = {
    slave(metaReq, dataReq, release)
    master(req)
    out(dataResp)
  }
}

case class NBDWriteBack()(implicit p: Parameters) extends NBDComponent {
  val io = slave(NBDWriteBackIO())

  val busy = Reg(Bool()) init false setWhen io.req.fire


}
