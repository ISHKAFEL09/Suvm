package rockets.params

import spinal.core._

trait DCacheParams extends CacheParams {
  val nSDQ: Int
  val nMSHRs: Int
  val nTLBs: Int
}

trait HasDCacheParams
    extends HasCacheParams
    with HasCoreParams
    with HasLinkParams
    with HasTileParams {
  override val cacheParams: DCacheParams = tileParams.dCache

  def outerDataBeats: Int = linkParams.TLDataBeats

  def outerDataBits: Int = linkParams.TLDataBits

  def refillCyclesPerBeat: Int = outerDataBits / rowBits

  def refillCycles: Int = refillCyclesPerBeat * outerDataBeats

  def wordBits: Int = coreParams.xLen

  def wordBytes: Int = wordBits / 8

  def wordOffBits: Int = log2Up(wordBytes)

  def sdqDepth: Int = cacheParams.nSDQ

  def rowWords: Int = rowBits / wordBits

  def nMSHRs: Int = cacheParams.nMSHRs

  def encDataBits: Int = code.width(coreDataBits)

  def encRowBits: Int = encDataBits * rowWords

  def tlbDepth: Int = cacheParams.nTLBs
}
