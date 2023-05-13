package uvm

trait UVMCoreService {
  def getUVMSeeding: Boolean

  def setUVMSeeding(enable: Boolean): Unit

  def getRoot: UVMRoot

  def getReportServer: UVMReportServer

  def getFactory: UVMFactory

  def setFactory(factory: UVMFactory): Unit
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
  private var reportServer: Option[UVMReportServer] = None
  private var factory: Option[UVMFactory] = None

  override def getUVMSeeding: Boolean = mUseUVMSeeding

  override def setUVMSeeding(enable: Boolean): Unit = mUseUVMSeeding = enable

  override def getRoot: UVMRoot = UVMRoot()

  override def getReportServer: UVMReportServer = reportServer match {
    case Some(value) => value
    case None =>
      reportServer = Some(new UVMDefaultReportServer)
      reportServer.get
  }

  override def getFactory: UVMFactory = factory match {
    case Some(value) => value
    case None =>
      factory = Some(new UVMDefaultFactory)
      factory.get
  }

  override def setFactory(factory: UVMFactory): Unit = this.factory = Some(factory)
}