package suvm

abstract class SuvmCoreService {
}

object SuvmCoreService {
  private var mUseUvmSeeding = true

  private lazy val mCopier = new SuvmCopier("SuvmDefaultCopier")

  def getSuvmSeeding: Boolean = mUseUvmSeeding
  def setSuvmSeeding(enable: Boolean) = mUseUvmSeeding = enable

  def getDefaultCopier: SuvmCopier = mCopier
}
