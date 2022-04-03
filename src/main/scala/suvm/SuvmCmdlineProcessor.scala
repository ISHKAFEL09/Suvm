package suvm

object SuvmCmdlineProcessor {
  type AT = Seq[String]
  private var _args: AT = Seq.empty[String]
  private var _suvmArgs: AT = Seq.empty[String]
  private var _plusArgs: AT = Seq.empty[String]

  private class SuvmCmdlineProcessorImpl(val name: String) extends SuvmReportObject {
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

  def getInst(implicit config: SuvmConfig): SuvmCmdlineProcessorImpl = _inst match {
    case Some(value) => value
    case None =>
      val i = new SuvmCmdlineProcessorImpl("SuvmCmdlineProc")
      _inst = Some(i)
      i
  }
}