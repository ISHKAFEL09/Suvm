package rockets.core.cache.nbdcache

import rockets.core.cache._
import rockets.params.config.Parameters
import rockets.tilelink._
import spinal.core._

/** meta array for nbdcache */

/** meta data store in tag mem, include tag and coh
  */
case class NBDMetaData()(implicit p: Parameters)
    extends NBDBundle
    with CacheMetaData {
  override val coh: HasCoherenceMetaData = ClientMetaData()
}

/** meta read, include set and tag
  */
case class NBDMetaReadReq()(implicit p: Parameters)
    extends NBDBundle
    with CacheMetaReadReq {
  val tag: UInt = UInt(tagBits bits)
}

/** meta write, plus write meta data and way
  */
case class NBDMetaWriteReq()(implicit p: Parameters)
    extends NBDBundle
    with CacheMetaWriteReq[NBDMetaData] {
  val data: NBDMetaData = NBDMetaData()
}

/** used in [[NBD]], based on [[CacheMetaData]], contains meta data array
  */
case class NBDMeta()(implicit p: Parameters)
    extends NBDComponent
    with CacheMeta[NBDMetaData] {
  override def data: HardType[NBDMetaData] = NBDMetaData()

  override def writeReq: HardType[CacheMetaWriteReq[NBDMetaData]] =
    new NBDMetaWriteReq()

  override def readReq: HardType[CacheMetaReadReq] = new NBDMetaReadReq()
}

object NBDMeta extends App {
  import rockets._
  import rockets.core.mmu.TLBApp
  generate(NBDMeta()(TLBApp.config))
}
