package rockets.core

import rockets.core.MemOps._
import rockets.params.HasDCacheParams
import rockets.params.config.Parameters
import rockets.tilelink._
import spinal.core._
import spinal.lib._

abstract class NBDCacheBundle(implicit val p: Parameters)
    extends Bundle
    with HasDCacheParams

abstract class NBDCacheComponent(implicit val p: Parameters)
    extends Component
    with HasDCacheParams

/** metadata for coherence, include tag and coh state
  */
trait MetaData extends NBDCacheBundle {
  val tag = UInt(tagBits bits)
  val coh: HasCoherenceMetaData
}

case class NBDMetaData()(implicit p: Parameters) extends MetaData {
  override val coh: HasCoherenceMetaData = ClientMetaData()
}

/** meta read, include set and way
  */
class MetaReadReq(implicit p: Parameters) extends NBDCacheBundle {
  val idx = UInt(idxBits bits)
  val way = UInt(nWays bits)
}

/** meta write, plus write meta data
  */
class MetaWriteReq(implicit p: Parameters) extends MetaReadReq {
  val data = NBDMetaData()
}

/** core <-> nbdcache
  */
trait HasCoreMemOp extends NBDCacheBundle {

  /** addr to visit */
  val addr = UInt(coreMaxAddrBits bits)
  // TODO: /** for replay? */
  val tag = UInt(coreDCacheRegTagBits bits)

  /** ld/st and others */
  val cmd = UInt(M_SZ bits)

  /** byte/half/word/double */
  val typ = UInt(MT_SZ bits)
}

/** core/replay <-> cache
  */
trait HasCoreData extends NBDCacheBundle {

  /** for ld/st? */
  val data = UInt(coreDataBits bits)
}

/** some control signals sent to cache
  */
trait HasCtrlInfo extends NBDCacheBundle {
  // TODO:  /** kill the previous request? */
  val kill = Bool()

  /** whether current request is ppn */
  val phys = Bool()
}

/** sdq_id to mshr & from replay
  */
trait HasSDQId extends NBDCacheBundle {

  /** store data queue, for replay st req */
  val sdqId = UInt(log2Up(sdqDepth) bits)
}

/** miss info to mshr
  */
trait HasMissInfo extends NBDCacheBundle {

  /** whether tag match */
  val tagMatch = Bool()

  /** meta data of miss entry */
  val oldMeta = NBDMetaData()

  /** way to be replaced */
  val way = UInt(nWays bits)
}

/** cache request
  */
case class NBDCacheReq()(implicit p: Parameters)
    extends HasCoreMemOp
    with HasCtrlInfo
    with HasCoreData

/** cache response
  */
case class NBDCacheResp()(implicit p: Parameters)
    extends HasCoreMemOp
    with HasCoreData {

  /** comes 2 cycles after req.fire */
  val nack = Bool()
  // TODO: complete the meanings of these signals
  val replay = Bool()
  val hasData = Bool()
  val subwordData = UInt(coreDataBits bits)
  val storeData = UInt(coreDataBits bits)
}

/** exceptions for cache
  */
case class NBDCacheExceptions()(implicit p: Parameters) extends NBDCacheBundle {

  /** misaligned exception from request to cache */
  val maXcpt = new Bundle {
    val ld, st = Bool()
  }

  /** exception from dtlb response */
  val dtlbXcpt = new Bundle {
    val ld, st = Bool()
  }
}

/** other module <-> NBDCache, cache as slave
  */
case class NBDCacheIO()(implicit p: Parameters)
    extends NBDCacheBundle
    with IMasterSlave {

  /** request to cache */
  val req = Stream(NBDCacheReq())

  /** cache response */
  val resp = Flow(NBDCacheResp())

  /** id for next replay req */
  val replayNext = Flow(UInt(coreDCacheRegTagBits bits))

  /** cache exceptions */
  val xcpt = NBDCacheExceptions()

  /** reset lr/sc counter */
  val invalidateLR = Bool()

  // TODO: meaning?
  val ordered = Bool()

  override def asMaster(): Unit = {
    master(req)
    slave(resp, replayNext)
    in(xcpt, ordered)
    out(invalidateLR)
  }
}

class NBDCache {}
