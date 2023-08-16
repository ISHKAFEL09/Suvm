package rockets.params

trait LinkParams {
  val pAddrBits: Int
  val vAddrBits: Int
  val pgIdxBits: Int
  val ppnBits: Int
  val vpnBits: Int
  val pgLevels: Int
  val asIdBits: Int
  val pgLevelBits: Int
  val TLDataBeats: Int
  val TLDataBits: Int
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

  def vAddrBitsExtended: Int = if (vAddrBits < xLen) vAddrBits + 1 else vAddrBits
}