package suvm
import java.io.File

abstract class SuvmComponent extends SuvmReportObject {
  def mDoPreAbort(): Unit = ???

  def doResolveBindings(): Unit = ???

  def getChildren: Seq[SuvmComponent] = ???

  def mSetClMsgArgs(): Unit = ???
}
