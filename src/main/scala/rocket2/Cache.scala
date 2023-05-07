package rocket2

import chisel3._
import chisel3.util._
import config._
import rocket2.tilelink.CoherenceMetadata

object CacheKey extends Field[CacheParams]
object Replacer extends Field[() => ReplacementPolicy]

trait CacheParams {
  def nSets: Int
  def blockOffBits: Int
  def nWays: Int
  def rowBits: Int
  def code: Option[Code]
}

object SimpleCacheParams extends CacheParams {
  override def nSets: Int = 32

  override def blockOffBits: Int = 6

  override def nWays: Int = 1

  override def rowBits: Int = 64

  override def code: Option[Code] = Some(new IdentityCode)
}

trait HasCacheParams {
  this: HasCoreParams =>

  def cacheParams: CacheParams = p(CacheKey)

  def nSets: Int = cacheParams.nSets
  def blockOffBits: Int = cacheParams.blockOffBits
  def idxBits: Int = log2Up(cacheParams.nSets)
  def untagBits: Int = blockOffBits + idxBits
  def tagBits: Int = pAddrBits - untagBits
  def nWays: Int = cacheParams.nWays
  def wayBits: Int = log2Up(nWays)
  def isDM: Boolean = nWays == 1
  def rowBits: Int = cacheParams.rowBits
  def rowBytes: Int = rowBits / 8
  def rowOffBits: Int = log2Up(rowBytes)
  def code = cacheParams.code.getOrElse(new IdentityCode)
}

abstract class CacheBundle(implicit val p: Parameters) extends Bundle with HasCacheParams with HasCoreParams
abstract class CacheModule(implicit val p: Parameters) extends Module with HasCacheParams with HasCoreParams

abstract class Metadata(implicit p: Parameters) extends CacheBundle()(p) {
  val tag = UInt(tagBits.W)
  val coh: CoherenceMetadata
}

abstract class ReplacementPolicy {
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(ways: Int) extends ReplacementPolicy {
  private val replace = WireInit(false.B)
  val lfsr = random.LFSR(16, replace)

  def way = if(ways == 1) 0.U else lfsr(log2Up(ways)-1,0)
  def miss = replace := true.B
  def hit = {}
}

class MetaReadReq(implicit p: Parameters) extends CacheBundle()(p) {
  val idx = UInt(idxBits.W)
}

class MetaWriteReq[T <: Metadata](gen: => T)(implicit p: Parameters) extends MetaReadReq()(p) {
  val wayEn = UInt(nWays.W)
  val data = gen
}

class MetaArray[T <: Metadata](rstVal: () => T)(implicit p: Parameters) extends CacheModule {
  val rst: T = rstVal()
  val metaWidth: Int = rst.getWidth

  val io = IO(new Bundle {
    val read: DecoupledIO[MetaReadReq] = Flipped(DecoupledIO(new MetaReadReq))
    val write: DecoupledIO[MetaWriteReq[T]] = Flipped(DecoupledIO(new MetaWriteReq(rst)))
    val resp: Vec[T] = Output(VecInit.fill(nWays)(chiselTypeOf(rst)))
  })

  val mem = SyncReadMem(nSets, Vec(nWays, UInt(metaWidth.W)))

  // initial
  val rstCnt = RegInit(0.U(log2Up(nSets + 1).W))
  val resetting: Bool = rstCnt < nSets.U
  when (resetting) {
    rstCnt := rstCnt + 1.U
  }

  val wrAddr: UInt = Mux(resetting, rstCnt, io.write.bits.idx)
  val wrData: UInt = Mux(resetting, rst, io.write.bits.data).asUInt
  val wrMask: UInt = Mux(resetting, (-1).S, io.write.bits.wayEn).asUInt
  when (resetting || io.write.fire) {
    mem.write(wrAddr, VecInit.fill(nWays)(wrData), wrMask.asBools)
  }
  io.resp := mem.read(io.read.bits.idx, io.read.fire)

  io.read.ready := !resetting && !io.write.valid
  io.write.ready := !resetting
}
