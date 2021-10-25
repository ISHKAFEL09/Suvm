package suvm

abstract class SuvmCoreService {
}

object SuvmCoreService {
  private var mUseUvmSeeding = true
  def getSuvmSeeding: Boolean = mUseUvmSeeding
  def setSuvmSeeding(enable: Boolean) = mUseUvmSeeding = enable

  private var mCopier = new SuvmCopier("SuvmDefaultCopier")
  def getDefaultCopier: SuvmCopier = mCopier

  private var mComparer = new SuvmComparer("SuvmDefaultComparer")
  def getDefaultComparer: SuvmComparer = mComparer

  private var mPacker = new SuvmPacker("SuvmDefaultPacker")
  def getDefaultPacker: SuvmPacker = mPacker

  private var mPrinter = SuvmTablePrinter.getDefault
  def getDefaultPrinter: SuvmPrinter = mPrinter
}
