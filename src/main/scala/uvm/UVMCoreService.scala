package uvm

trait UVMCoreService {
  def getUVMSeeding: Boolean

  def setUVMSeeding(enable: Boolean): Unit

  def getRoot: UVMRoot
}

object UVMCoreService {
  private var inst: Option[UVMCoreService] = None

  private[uvm] def apply(): UVMCoreService = inst match {
    case Some(s) => s
    case None =>
      uvmInit()
      require(inst.nonEmpty)
      inst.get
  }

  private[uvm] def set(cs: UVMCoreService): Unit = inst = Some(cs)
}

class UVMDefaultCoreService extends UVMCoreService {
  private var mUseUVMSeeding: Boolean = true

  override def getUVMSeeding: Boolean = mUseUVMSeeding

  override def setUVMSeeding(enable: Boolean): Unit = mUseUVMSeeding = enable

  override def getRoot: UVMRoot = UVMRoot()
}