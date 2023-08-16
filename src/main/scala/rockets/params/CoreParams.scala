package rockets.params

trait CoreParams {
  val xLen: Int
  val retireWidth: Int
  val coreFetchWidth: Int
  val coreInstBits: Int
  val coreDCacheRegTagBits: Int
  val fastLoadByte: Boolean
  val fastLoadWord: Boolean
  val maxHartIdBits: Int
}

trait HasCoreParams {
  this: HasTileParams =>

  val coreParams: CoreParams = tileParams.core

  def xLen: Int = coreParams.xLen

  def hartIdLen: Int = coreParams.maxHartIdBits

  def retireWidth: Int = coreParams.retireWidth

  def coreFetchWidth: Int = coreParams.coreFetchWidth

  def coreInstBits: Int = coreParams.coreInstBits

  def coreInstBytes: Int = coreInstBits / 8

  def coreDataBits: Int = xLen

  def coreDataBytes: Int = coreDataBits / 8

  def coreDCacheRegTagBits: Int = coreParams.coreDCacheRegTagBits

  def fastLoadByte: Boolean = coreParams.fastLoadByte

  def fastLoadWord: Boolean = coreParams.fastLoadWord

  if (fastLoadByte) require(fastLoadWord)
}