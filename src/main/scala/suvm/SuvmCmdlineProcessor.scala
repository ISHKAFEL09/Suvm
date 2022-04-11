package suvm

import scala.collection.mutable.ArrayBuffer

trait SuvmCmdlineProcessor extends SuvmReportObject {
  type AT = ArrayBuffer[String]

  def getArgMatches(pattern: String): AT
}

object SuvmCmdlineProcessor {
  private class SuvmCmdlineProcessorImpl(name: String) extends SuvmCmdlineProcessor {
    private var _args = ArrayBuffer.empty[String]
    private var _suvmArgs = ArrayBuffer.empty[String]
    private var _plusArgs = ArrayBuffer.empty[String]

    def init(args: AT): Unit = {
      args foreach { i =>
        _args += i
        if (i.startsWith("+")) _plusArgs += i
        if (suvmIsMatch("[+-]uvm.+?", i)) _suvmArgs += i
      }
    }

    def setArgs(args: AT): Unit = _args = args

    def getArgs: AT = _args

    def getSuvmArgs: AT = _suvmArgs

    def getPlusArgs: AT = _plusArgs

    def getArgMatches(pattern: String): AT = {
      getArgs.filter(suvmIsMatch(pattern, _))
    }

    def getArgValue(pattern: String): Option[String] = {
      getArgs.find(_.startsWith(pattern)).map(_.substring(pattern.length))
    }

    def getArgValues(pattern: String): AT = {
      getArgs.filter(_.startsWith(pattern)).map(_.substring(pattern.length))
    }

    def getToolName: String = "SUVM"

    def getToolVersion: String = "0.1"
  }

  private var _inst: Option[SuvmCmdlineProcessorImpl] = None

  def getInst(implicit config: SuvmConfig): SuvmCmdlineProcessor = _inst match {
    case Some(value) => value
    case None =>
      val i = new SuvmCmdlineProcessorImpl("SuvmCmdlineProc")
      _inst = Some(i)
      i
  }
}