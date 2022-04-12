package suvm
import java.io.File

abstract class SuvmComponent(name: String = "",
                             val parent: Option[SuvmComponent] = None) extends SuvmReportObject {
  def mDoPreAbort(): Unit = ???

  def doResolveBindings(): Unit = ???

  def getChildren: Seq[SuvmComponent] = ???

  def mSetClMsgArgs(): Unit = {}

  def getParent: Option[SuvmComponent] = parent
}
