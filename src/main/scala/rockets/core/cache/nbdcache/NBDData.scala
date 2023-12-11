package rockets.core.cache.nbdcache

import rockets.core.cache._
import rockets.params.config.Parameters
import spinal.core._

/** cache data read, include addr and way
  */
case class NBDDataReadReq()(implicit p: Parameters)
    extends NBDBundle
    with CacheDataReadReq

/** data write, plus write data and mask
  */
case class NBDDataWriteReq()(implicit p: Parameters)
    extends NBDBundle
    with CacheDataWriteReq

/** used in [[NBD]], based on [[CacheData]], contains cache data array
  */
case class NBDData()(implicit p: Parameters)
    extends NBDComponent
    with CacheData[NBDDataReadReq, NBDDataWriteReq] {

  /** read request port */
  override def readReq: HardType[NBDDataReadReq] = NBDDataReadReq()

  /** write request port */
  override def writeReq: HardType[NBDDataWriteReq] = NBDDataWriteReq()
}

object NBDData extends App {
  import rockets._
  import rockets.tile.Configs._
  generate(NBDData())
}
