package suvm

import SuvmObjectGlobals._
import SuvmObjectGlobals.SuvmCoreState._
import SuvmImplicits._

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

abstract class SuvmRoot(name: String, parent: Option[SuvmComponent]) extends SuvmComponent(name, parent) {
  var phaseTimeout: Time = SuvmDefaultTimeout
  var finishOnCompletion: Boolean = true
  val mPhaseAllDone: Promise[Unit] = Promise[Unit]
  override def getTypeName: String = "SuvmRoot"
  def runTest(testName: String): Unit
  def die(): Unit
}

object SuvmRoot {
  private class SuvmRootImpl(name: String)(implicit config: SuvmConfig) extends SuvmRoot(name, None) {
    /** simulation control
     */
    def runTest(testName: String): Unit = {

      Await.result(mPhaseAllDone.future, Duration.Inf)
    }

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

    /**
     * topology
     */
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
      adapter.accept(getInst, v, p)
    }

    def reportHeader(file: Option[File] = None): Unit = {
      val args = SuvmCmdlineProcessor.getInst.getArgMatches("+UVM_NO_RELNOTES")
      val q = ArrayBuffer.empty[String]

      if (args.isEmpty) {
        if (!mRelnotesDone) {
          q += "\n  ***********       IMPORTANT RELEASE NOTES         ************\n"
          mRelnotesDone = true
          q += "\n  This implementation of the UVM Library deviates from the 1800.2-2020\n"
          q += "  standard.  See the DEVIATIONS.md file contained in the release\n"
          q += "  for more details.\n"
          if (config.SUVM_ENABLE_DEPRECATED_API) {
            q += "\n  You are using a version of the UVM library that has been compiled\n"
            q += "  with `UVM_ENABLE_DEPRECATED_API defined.\n"
            q += "  See https://accellera.mantishub.io/view.php?id=5072 for more details.\n"
          }
          q += "\n----------------------------------------------------------------\n"
          q += s"$SUVM_REVERSION\n"
          q += "\n"
          q += "All copyright owners for this kit are listed in NOTICE.txt\n"
          q += "All Rights Reserved Worldwide\n"
          q += "----------------------------------------------------------------\n"
          if (mRelnotesDone)
            q += "\n      (Specify +UVM_NO_RELNOTES to turn off this notice)\n"
        }
      }
      suvmInfo("UVM/RELNOTES", q.mkString, SuvmVerbosity.UVM_LOW)
    }

    def mDoCmdlineChecks(): Unit = ???

    def mDoClInst(): Unit = {}

    def mFindAllRecurse(compMatch: String, comp: Option[SuvmComponent] = None): List[SuvmComponent] =
      ???

    setReportHandler(new SuvmReportHandler("reporter"))
    mDoClInst()
    mSetClMsgArgs()
  }

  private var root: Option[SuvmRoot] = None
  private var enablePrintTopology: Boolean = false
  private var mRelnotesDone: Boolean = false

  private[suvm] def getInst: SuvmRoot = root.getOrElse(throw new RuntimeException("no root"))

  private[suvm] def init(implicit config: SuvmConfig): SuvmRoot = root match {
    case Some(value) => value
    case None =>
      val r = new SuvmRootImpl("__top__")
      root = Some(r)
      r
  }

  def get: SuvmRoot = getInst
}