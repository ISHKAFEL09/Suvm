package suvm

abstract class SuvmCoreService {
}

object SuvmCoreService {
  private var mUseUvmSeeding = true

  def getSuvmSeeding: Boolean = mUseUvmSeeding

  def setSuvmSeeding(enable: Boolean) = mUseUvmSeeding = enable

  private val mCopier = new SuvmCopier("SuvmDefaultCopier")

  def getDefaultCopier: SuvmCopier = mCopier

  private val mComparer = new SuvmComparer("SuvmDefaultComparer")

  def getDefaultComparer: SuvmComparer = mComparer

  private val mPacker = new SuvmPacker("SuvmDefaultPacker")

  def getDefaultPacker: SuvmPacker = mPacker

  private val mPrinter = SuvmTablePrinter.getDefault

  def getDefaultPrinter: SuvmPrinter = mPrinter

  private var reportServer: SuvmReportServer = new SuvmDefaultReportServer

  def getReportServer: SuvmReportServer = reportServer

  def setReportServer(server: SuvmReportServer): Unit = reportServer = server

  private var visitor: SuvmVisitor[SuvmComponent] = new SuvmComponentNameCheckVisitor("nameCheckVisitor")

  def setComponentVisitor(v: SuvmVisitor[SuvmComponent]): Unit = visitor = v

  def getComponentVisitor: SuvmVisitor[SuvmComponent] = visitor

  private val mTrDatabase = new SuvmTrDatabase("DefaultTrDatabase")

  def getDefaultTrDatabase: SuvmTrDatabase = mTrDatabase
}
