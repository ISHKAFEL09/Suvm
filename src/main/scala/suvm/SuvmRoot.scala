package suvm

import SuvmObjectGlobals._
import SuvmObjectGlobals.SuvmCoreState._

object SuvmRoot {

  private class SuvmRootImpl(name: String) extends SuvmComponent(name) {
    val rh: SuvmReportHandler = new SuvmReportHandler("reporter")
    val clp: SuvmCmdlineProcessor.SuvmCmdlineProcessorImpl = SuvmCmdlineProcessor.getInst
    var phaseTimeout: Time = SuvmDefaultTimeout
    var finishOnCompletion: Boolean = true

    setReportHandler(rh)
    mDoClInst()
    mSetClMsgArgs()

    def mDoClInst(): Unit = {}

    def mSetClMsgArgs(): Unit = {}

    override def getTypeName: String = "SuvmRoot"

    def mDoCmdlineChecks(): Unit = ???

    def runTest(testName: String): Unit = {}

    def die(): Unit = {
      if (!(mSuvmCoreState == UVM_CORE_PRE_ABORT || mSuvmCoreState == UVM_CORE_ABORTED)) {
        val lRs = SuvmReportServer.getServer
        mSuvmCoreState = UVM_CORE_PRE_ABORT
        mDoPreAbort()
        SuvmRunTestCallback.mDoPreAbort()
        mDoCmdlineChecks()
        lRs.reportSummarize()
        mSuvmCoreState = UVM_CORE_ABORTED
      }
    }

    def setTimeout(timeout: Time, overridable: Boolean = true): Unit

    def getFinishOnCompletion: Boolean = finishOnCompletion

    def find(compMatch: String): SuvmComponent

    def findAll(compMatch: String, comp: SuvmComponent): List[SuvmComponent]

    def printTopology(printer: Option[SuvmPrinter] = None): Unit

    def setEnablePrintTopology(enable: Boolean): Unit

    def getEnablePrintTopology: Boolean
  }

  private lazy val root = new SuvmRootImpl("__top__")
  private var enablePrintTopology: Boolean = false

  def get: SuvmRootImpl = root
}