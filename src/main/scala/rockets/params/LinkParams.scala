package rockets.params

import rockets.tilelink._
import spinal.core.log2Up

trait LinkParams {
  val pAddrBits: Int
  val vAddrBits: Int
  val pgIdxBits: Int
  val ppnBits: Int
  val vpnBits: Int
  val pgLevels: Int
  val asIdBits: Int
  val pgLevelBits: Int

  /** unique name per TL network */
  val TLId: String

  /** coherency policy, like MOESI */
  val coherencePolicy: CoherencePolicy

  /** manager agents number for this network */
  val TLManagerNum: Int

  /** client agents number for this network */
  val TLClientNum: Int

  /** number of client agents that cache data */
  val TLCacheClientNum: Int

  /** number of client agents that do not cache data */
  val TLNoCacheClientNum: Int

  /** maximum number of outstanding xact per client */
  val TLMaxClientOst: Int

  /** maximum number of clients multiplexed onto one port */
  val TLMaxClientsPerPort: Int

  /** maximum number of outstanding xact per manager */
  val TLMaxManagerOst: Int

  /** width of data per beat */
  val TLDataBits: Int

  /** number of beats per block */
  val TLDataBeats: Int

  /** width of cache block address */
  val TLBlockAddrBits: Int

  /** amo alu op size */
  val AmoOperandBits: Int
}

trait HasLinkParams {
  this: HasTileParams with HasCoreParams =>

  val linkParams: LinkParams = tileParams.link

  def pAddrBits: Int = linkParams.pAddrBits

  def vAddrBits: Int = linkParams.vAddrBits

  def pgIdxBits: Int = linkParams.pgIdxBits

  def ppnBits: Int = linkParams.ppnBits

  def vpnBits: Int = linkParams.vpnBits

  def pgLevels: Int = linkParams.pgLevels

  def pgLevelBits: Int = linkParams.pgLevelBits

  def asIdBits: Int = linkParams.asIdBits

  def coreMaxAddrBits: Int = math.max(ppnBits, vpnBits + 1) + pgIdxBits

  def vAddrBitsExtended: Int =
    if (vAddrBits < xLen) vAddrBits + 1 else vAddrBits

  def tlCoh = linkParams.coherencePolicy

  def tlManagerNum: Int = linkParams.TLManagerNum

  def tlClientNum: Int = linkParams.TLClientNum

  def tlCacheClientNum: Int = linkParams.TLCacheClientNum

  def tlClientIdBits: Int = log2Up(tlClientNum)

  def tlMaxClientOst: Int = linkParams.TLMaxClientOst

  def tlMaxClientsPerPort: Int = linkParams.TLMaxClientsPerPort

  def tlMaxManagerOst: Int = linkParams.TLMaxManagerOst

  def tlClientXactIdBits: Int = log2Up(tlMaxClientOst * tlMaxClientsPerPort)

  def tlManagerXactIdBits: Int = log2Up(tlMaxManagerOst)

  def tlBlockAddrBits: Int = linkParams.TLBlockAddrBits

  def tlDataBits: Int = linkParams.TLDataBits

  def tlDataBeats: Int = linkParams.TLDataBeats

  def tlWriteMaskBits: Int = if (tlDataBits < 8) 1 else tlDataBits / 8

  def tlBeatAddrBits: Int = log2Up(tlDataBeats)

  def tlByteAddrBits: Int = log2Up(tlWriteMaskBits)

  def amoOperandBits: Int = linkParams.AmoOperandBits
}
