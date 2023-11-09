package rockets.core.cache.nbdcache

import rockets.core.cache._
import rockets.params.config.Parameters
import rockets.tilelink._
import spinal.core._
import spinal.lib._

/** meta array for nbdcache */

/** meta data store in tag mem, include tag and coh
  */
case class NBDMetaData()(implicit p: Parameters)
    extends NBDCacheBundle
    with MetaData {
  val tag: UInt = UInt(tagBits bits)
  override val coh: HasCoherenceMetaData = ClientMetaData()
}

/** meta read, include set and tag
  */
class NBDMetaReadReq(implicit p: Parameters)
    extends NBDCacheBundle
    with MetaReadReq {
  val idx: UInt = UInt(idxBits bits)
  val tag: UInt = UInt(tagBits bits)
}

/** meta write, plus write meta data and way
  */
class NBDMetaWriteReq(implicit p: Parameters)
    extends NBDCacheBundle
    with MetaWriteReq[NBDMetaData] {
  val idx: UInt = UInt(idxBits bits)
  val data: NBDMetaData = NBDMetaData()
  val way: UInt = UInt(nWays bits)
}

/** used in [[NBDCache]], based on [[Meta]], contains meta data array
  */
case class NBDMeta()(implicit p: Parameters)
    extends NBDCacheComponent
    with Meta[NBDMetaData] {
  override def data: HardType[NBDMetaData] = NBDMetaData()

  override def writeReq: HardType[MetaWriteReq[NBDMetaData]] =
    new NBDMetaWriteReq()

  override def readReq: HardType[MetaReadReq] = new NBDMetaReadReq()
}

object NBDMeta extends App {
  import rockets._
  import rockets.core._
  generate(NBDMeta()(TLBApp.config))
}
