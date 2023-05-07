package suvm

import SuvmObjectGlobals._
import SuvmImplicits._
import java.io.File

// TODO: how to realize callback facility?
class SuvmReportCatcher(name: String) extends SuvmCallback(name) {

}

object SuvmReportCatcher {
  def summarize(file: Option[File]): Unit = ???

  def processAllReportCatchers(reportMessage: SuvmReportMessage): Boolean = true
}