package suvm

import java.io.File

abstract class SuvmReportCatcher(name: String) extends SuvmCallback(name) {

}

object SuvmReportCatcher {
  def summarize(file: Option[File]): Unit = ???

  def processAllReportCatchers(reportMessage: SuvmReportMessage): Boolean = true
}