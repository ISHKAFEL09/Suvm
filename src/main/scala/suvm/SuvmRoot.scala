package suvm

import SuvmObjectGlobals._
import SuvmObjectGlobals.SuvmCoreState._
import java.io.File
import scala.collection.mutable.ArrayBuffer

object SuvmRoot {
  private class SuvmRootImpl(val name: String)(implicit config: SuvmConfig) extends SuvmComponent {
    var phaseTimeout: Time = SuvmDefaultTimeout
    var finishOnCompletion: Boolean = true
    private var mPhaseAllDone: Boolean = false

    override def getTypeName: String = "SuvmRoot"

    def runTest(testName: String): Unit = {}

    def die(): Unit = {
      if (!(mSuvmCoreState == UVM_CORE_PRE_ABORT || mSuvmCoreState == UVM_CORE_ABORTED)) {
        mSuvmCoreState = UVM_CORE_PRE_ABORT
        mDoPreAbort()
        SuvmRunTestCallback.mDoPreAbort()
        mDoCmdlineChecks()
        SuvmReportServer.getServer.reportSummarize()
        mSuvmCoreState = UVM_CORE_ABORTED
      }
    }

    def setTimeout(timeout: Time, overridable: Boolean = true): Unit =
      ???

    def getFinishOnCompletion: Boolean = finishOnCompletion

    def find(compMatch: String): SuvmComponent =
      ???

    def findAll(compMatch: String, comp: SuvmComponent): List[SuvmComponent] =
      ???

    def printTopology(printer: Option[SuvmPrinter] = None): Unit =
      ???

    def setEnablePrintTopology(enable: Boolean): Unit =
      ???

    def getEnablePrintTopology: Boolean =
      ???

    def phaseStarted(phase: SuvmPhase): Unit = {
      if (phase == SuvmEndOfElaborationPhase) {
        doResolveBindings()
        if (enablePrintTopology) printTopology()
        if (SuvmReportServer.getServer.getSeverityCount(SuvmSeverity.UVM_ERROR) > 0)
          suvmReportFatal("BUILDERR", "stopping due to build errors", SuvmVerbosity.UVM_NONE)
      }
    }

    def endOfElaborationPhase(phase: SuvmPhase): Unit = {
      val p = new SuvmComponentProxy("proxy")
      val adapter = new SuvmTopDownVisitorAdapter("adapter")
      val v = SuvmCoreService.getComponentVisitor
      adapter.accept(get, v, p)
    }

    def reportHeader(file: Option[File] = None): Unit = {
      val args = SuvmCmdlineProcessor.getInst.getArgMatches("+UVM_NO_RELNOTES")
      val q = ArrayBuffer.empty[String]

      if (args.isEmpty) {
        if (!mRelnotesDone) {
          q += "\\n  ***********       IMPORTANT RELEASE NOTES         ************\\n"
          mRelnotesDone = true
          if (config.SUVM_ENABLE_DEPRECATED_API) {

          }
        }
      }
      suvmInfo("UVM/RELNOTES", q.mkString, SuvmVerbosity.UVM_LOW)
    }

    private def mDoCmdlineChecks(): Unit = ???

    private def mDoClInst(): Unit = {}

    private def mFindAllRecurse(compMatch: String, comp: Option[SuvmComponent] = None): List[SuvmComponent] =
      ???

    setReportHandler(new SuvmReportHandler("reporter"))
    mDoClInst()
    mSetClMsgArgs()
  }

  private var root: Option[SuvmRootImpl] = None
  private var enablePrintTopology: Boolean = false
  private var mRelnotesDone: Boolean = false

  def getInst: SuvmRootImpl = root.getOrElse(throw new RuntimeException("no root"))

  def get(implicit config: SuvmConfig): SuvmRootImpl = root match {
    case Some(value) => value
    case None =>
      val r = new SuvmRootImpl("__top__")
      root = Some(r)
      r
  }
}