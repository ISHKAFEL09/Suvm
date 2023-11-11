package rockets.core.cache.nbdcache

import rockets.core.cache._
import rockets.params.config.Parameters
import rockets.tilelink._
import spinal.core._

/** meta array for nbdcache */

/** meta data store in tag mem, include tag and coh
  */
case class NBDCacheMetaData()(implicit p: Parameters)
    extends NBDCacheBundle
    with CacheMetaData {
  override val coh: HasCoherenceMetaData = ClientMetaData()
}

/** meta read, include set and tag
  */
class NBDCacheMetaReadReq(implicit p: Parameters)
    extends NBDCacheBundle
    with CacheMetaReadReq {
  val tag: UInt = UInt(tagBits bits)
}

/** meta write, plus write meta data and way
  */
class NBDCacheMetaWriteReq(implicit p: Parameters)
    extends NBDCacheBundle
    with CacheMetaWriteReq[NBDCacheMetaData] {
  val data: NBDCacheMetaData = NBDCacheMetaData()
}

/** used in [[NBDCache]], based on [[CacheMetaData]], contains meta data array
  */
case class NBDCacheMeta()(implicit p: Parameters)
    extends NBDCacheComponent
    with CacheMeta[NBDCacheMetaData] {
  override def data: HardType[NBDCacheMetaData] = NBDCacheMetaData()

  override def writeReq: HardType[CacheMetaWriteReq[NBDCacheMetaData]] =
    new NBDCacheMetaWriteReq()

  override def readReq: HardType[CacheMetaReadReq] = new NBDCacheMetaReadReq()
}

object NBDCacheMeta extends App {
  import rockets._
  import rockets.core.mmu.TLBApp
  generate(NBDCacheMeta()(TLBApp.config))
}
