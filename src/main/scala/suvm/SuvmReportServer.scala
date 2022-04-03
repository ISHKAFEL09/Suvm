package suvm

import SuvmObjectGlobals._
import SuvmObjectGlobals.SuvmVerbosity._
import SuvmObjectGlobals.SuvmSeverity._

import java.io.File
import scala.collection.mutable

abstract class SuvmReportServer extends SuvmObject {
  def reportSummarize(f: File = new File(UVM_STDOUT)): Unit
  def getSeverityCount(severity: SuvmSeverity.Value): Int
}

class SuvmDefaultReportServer(val name: String = "SuvmReportServer") extends SuvmReportServer {
  private var mMaxQuitCount = 0
  private var mQuitCount = 0
  private val mSeverityCount = collection.mutable.HashMap.empty[SuvmSeverity.Value, Int]
  private val mIdCount = collection.mutable.HashMap.empty[String, Int]
  val q: mutable.Queue[String] = collection.mutable.Queue.empty[String]
  var enableReportIdCountSummary = true

  def reportSummarize(f: File = new File(UVM_STDOUT)): Unit = {
    SuvmReportCatcher.summarize(f)
    q.enqueue("\n--- UVM Report Summary ---\n\n")

    if (mMaxQuitCount != 0) {
      if (mQuitCount >= mMaxQuitCount) {
        q.enqueue("Quit count reached!\n")
      }
      q.enqueue("Quit count : %5d of %5d\n".format(mQuitCount, mMaxQuitCount))
    }

    q.enqueue("** Report counts by severity\n")
    mSeverityCount.keys.foreach(i => q.enqueue("%s :%5d\n".format(i.toString, mSeverityCount(i))))

    if (enableReportIdCountSummary) {
      q.enqueue("** Report counts by id\n")
      mIdCount.keys.foreach(i => q.enqueue("[%s] %5d\n".format(i, mIdCount(i))))
    }

    if (f.getName == UVM_STDOUT)
      suvmInfo("UVM/REPORT/SERVER", "UVM_STRING_QUEUE_STREAMING_PACK", UVM_NONE) // TODO:
    else {
      val root = SuvmRoot.getInst
      val action = root.getReportAction(UVM_INFO, "UVM/REPORT/SERVER")
      root.setReportIdAction("UVM/REPORT/SERVER", SuvmActionType.UVM_LOG)
      root.setReportIdFile("UVM/REPORT/SERVER", f)
      suvmInfo("UVM/REPORT/SERVER", "UVM_STRING_QUEUE_STREAMING_PACK", UVM_NONE) // TODO:
      root.setReportIdAction("UVM/REPORT/SERVER", action)
    }
  }

  def getSeverityCount(severity: SuvmSeverity.Value): Int = mSeverityCount(severity)
}

object SuvmReportServer {
  def getServer: SuvmReportServer = SuvmCoreService.getReportServer
}