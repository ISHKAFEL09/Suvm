package rockets.core.cache

import rockets.params._
import rockets.params.config.Parameters
import rockets.tilelink._
import spinal.core._
import spinal.lib._
import spinal.lib.misc._

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
trait CacheMetaData extends CacheBundle {
  val tag: UInt = UInt(tagBits bits)
  val coh: HasCoherenceMetaData
}

/** meta read, include set and tag
  */
trait CacheMetaReadReq extends CacheBundle {
  val idx: UInt = UInt(idxBits bits)
}

/** meta write, plus write meta data and way
  */
trait CacheMetaWriteReq[T <: CacheMetaData] extends CacheMetaReadReq {
  val data: T
  val way: UInt = UInt(nWays bits)
}

/** basic meta data array component
  */
trait CacheMeta[T <: CacheMetaData] extends CacheComponent {

  /** concrete meta data */
  def data: HardType[T]

  private def resetData: T = {
    val r = data()
    r.assignFromBits(B(0, widthOf(r) bits))
    r
  }

  /** sub class of [[CacheMetaReadReq]] */
  def readReq: HardType[CacheMetaReadReq]

  /** sub class of [[CacheMetaWriteReq]] */
  def writeReq: HardType[CacheMetaWriteReq[T]]

  val io = new Bundle {
    val read = slave(Stream(readReq()))
    val write = slave(Stream(writeReq()))
    val resp = out port Vec(data(), nWays)
  }

  /** tag memory [[nSets]] * [[nWays]] */
  val tagMem = Mem(Vec(data(), nWays), nSets)

  /** reset all sets, need nSets cycles */
  val resetCounter = Counter(nSets + 1)
  when(resetCounter.value < nSets) {
    resetCounter.increment()
  }
  val waitResetDone = resetCounter.willIncrement

  /** write rst value or request value to memory */
  when(waitResetDone) {
    tagMem.write(resetCounter.resized, Vec(resetData, nWays))
  }.elsewhen(io.write.valid) {
    tagMem.write(
      io.write.payload.idx,
      Vec(io.write.payload.data, nWays),
      mask = io.write.payload.way.asBits
    )
  }
  io.write.ready := !waitResetDone

  /** read tag value from memory */
  io.resp := tagMem.readSync(io.read.payload.idx, io.read.valid)
  io.read.ready := !waitResetDone && !io.write.valid
}

/** cache data read */
trait CacheDataReadReq extends CacheBundle {
  val addr = UInt(untagBits bits)
  val way = UInt(nWays bits)
}

/** cache data write */
trait CacheDataWriteReq extends CacheDataReadReq {
  def encRowBits: Int
  def rowWords: Int

  val data = UInt(encRowBits bits)
  val mask = UInt(rowWords bits)
}

/** basic cache data component */
trait CacheData extends CacheComponent {

  /** read request port */
  def readReq: HardType[CacheDataReadReq]

  /** write request port */
  def writeReq: HardType[CacheDataWriteReq]

  def refillCycles: Int

  val io = new Bundle {
    val read = slave(Stream(readReq()))
    val write = slave(Stream(writeReq()))
    val resp = out port Vec(UInt(write.encRowBits bits), nWays)
  }

  /** data memory */
  val dataMem = Seq.fill(nWays)(
    Mem(UInt(io.write.encRowBits bits), nSets * refillCycles)
  )

  /** data write & read */
  dataMem.zipWithIndex.foreach { case (mem, way) =>
    mem.write(
      io.write.payload.addr >> rowOffBits,
      io.write.payload.data,
      enable = io.write.valid && io.write.payload.way(way),
      mask = io.write.payload.mask.asBits
    )

    io.resp(way) := mem.readSync(
      io.read.payload.addr >> rowOffBits,
      enable = io.read.valid && io.read.payload.way(way)
    )
  }

  io.write.ready := True
  io.read.ready := True
}
