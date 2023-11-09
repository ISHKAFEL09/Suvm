package rockets.core.cache

import rockets.params._
import rockets.params.config.Parameters
import rockets.tilelink._
import spinal.core._
import spinal.lib._

/** some basic traits for cache */

abstract class CacheBundle(implicit val p: Parameters)
    extends Bundle
    with HasCacheParams
    with HasLinkParams
    with HasCoreParams
    with HasTileParams

abstract class CacheComponent(implicit val p: Parameters)
    extends Component
    with HasCacheParams
    with HasLinkParams
    with HasCoreParams
    with HasTileParams

/** metadata for coherence, include tag and coh state
  */
trait MetaData extends CacheBundle {
  val tag: UInt
  val coh: HasCoherenceMetaData
}

/** meta read, include set and tag
  */
trait MetaReadReq extends CacheBundle {
  val idx: UInt
}

/** meta write, plus write meta data and way
  */
trait MetaWriteReq[T <: MetaData] extends MetaReadReq {
  val data: T
  val way: UInt
}

/** basic meta data array component
  */
trait Meta[T <: MetaData] extends CacheComponent {

  /** concrete meta data */
  def data: HardType[T]

  /** sub class of [[MetaReadReq]] */
  def readReq: HardType[MetaReadReq]

  /** sub class of [[MetaWriteReq]] */
  def writeReq: HardType[MetaWriteReq[T]]

  val io = new Bundle {
    val read = slave(Stream(readReq()))
    val write = slave(Stream(writeReq()))
    val resp = out port Vec(data(), nWays)
  }
}
