package rockets.core.cache.nbdcache

import rockets.core.cache._
import rockets.params.config.Parameters
import spinal.core._

/** cache data read, include addr and way
  */
class NBDCacheDataReadReq(implicit p: Parameters)
    extends NBDCacheBundle
    with CacheDataReadReq

/** data write, plus write data and mask
  */
class NBDCacheDataWriteReq(implicit p: Parameters)
    extends NBDCacheBundle
    with CacheDataWriteReq

/** used in [[NBDCache]], based on [[CacheData]], contains cache data array
  */
case class NBDData()(implicit p: Parameters)
    extends NBDCacheComponent
    with CacheData {

  /** read request port */
  override def readReq: HardType[CacheDataReadReq] =
    new NBDCacheDataReadReq()

  /** write request port */
  override def writeReq: HardType[CacheDataWriteReq] =
    new NBDCacheDataWriteReq()
}

object NBDCacheData extends App {
  import rockets._
  import rockets.core.mmu.TLBApp
  generate(NBDData()(TLBApp.config))
}
