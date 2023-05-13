package object uvm {
  // private, for uvm
  private[uvm] def getTrace(level: Int = 2): String = {
    new Throwable().getStackTrace()(level).toString
  }

  private[uvm] def uvmInit(cs: Option[UVMCoreService] = None): Unit = {
    // TODO:
    UVMCoreService.set(new UVMDefaultCoreService)
    val top = UVMRoot()
    uvmFatal("Init", "this it uvmInit")
  }

  // public
  object ENUM_UVM_SEVERITY extends Enumeration {
    val UVM_INFO, UVM_WARNING, UVM_ERROR, UVM_FATAL = Value
  }

  type uvmSeverity = ENUM_UVM_SEVERITY.Value

  def UVM_INFO: uvmSeverity = ENUM_UVM_SEVERITY.UVM_INFO

  def UVM_WARNING: uvmSeverity = ENUM_UVM_SEVERITY.UVM_WARNING

  def UVM_ERROR: uvmSeverity = ENUM_UVM_SEVERITY.UVM_ERROR

  def UVM_FATAL: uvmSeverity = ENUM_UVM_SEVERITY.UVM_FATAL

  object ENUM_UVM_VERBOSITY extends Enumeration {
    val UVM_NONE = Value(0)
    val UVM_LOW = Value(100)
    val UVM_MEDIUM = Value(200)
    val UVM_HIGH = Value(300)
    val UVM_FULL = Value(400)
    val UVM_DEBUG = Value(500)
  }

  type uvmVerbosity = ENUM_UVM_VERBOSITY.Value

  def UVM_NONE: uvmVerbosity = ENUM_UVM_VERBOSITY.UVM_NONE

  def UVM_LOW: uvmVerbosity = ENUM_UVM_VERBOSITY.UVM_LOW

  def UVM_MEDIUM: uvmVerbosity = ENUM_UVM_VERBOSITY.UVM_MEDIUM

  def UVM_HIGH: uvmVerbosity = ENUM_UVM_VERBOSITY.UVM_HIGH

  def UVM_FULL: uvmVerbosity = ENUM_UVM_VERBOSITY.UVM_FULL

  def UVM_DEBUG: uvmVerbosity = ENUM_UVM_VERBOSITY.UVM_DEBUG

  lazy val uvmReportEnabled: (uvmVerbosity, uvmSeverity, String) => Boolean =
    UVMCoreService().getRoot.uvmReportEnabled

  lazy val uvmReport: (uvmSeverity, String, String, uvmVerbosity, String, String, Boolean) => Unit =
    UVMCoreService().getRoot.uvmReport

  lazy val uvmReportInfo: (String, String, uvmVerbosity, String, String, Boolean) => Unit =
    UVMCoreService().getRoot.uvmReportInfo

  lazy val uvmReportWarning: (String, String, uvmVerbosity, String, String, Boolean) => Unit =
    UVMCoreService().getRoot.uvmReportWarning

  lazy val uvmReportFatal: (String, String, uvmVerbosity, String, String, Boolean) => Unit =
    UVMCoreService().getRoot.uvmReportFatal

  lazy val uvmInfo: (String, String, uvmVerbosity) => Unit =
    UVMCoreService().getRoot.uvmInfo

  lazy val uvmWarning: (String, String) => Unit =
    UVMCoreService().getRoot.uvmWarning

  lazy val uvmFatal: (String, String) => Unit =
    UVMCoreService().getRoot.uvmFatal

  object ENUM_UVM_ACTION extends Enumeration {
    val UVM_NO_ACTION = Value
    val UVM_DISPLAY = Value
    val UVM_LOG = Value
    val UVM_COUNT = Value
    val UVM_EXIT = Value
    val UVM_CALL_HOOK = Value
    val UVM_STOP = Value
    val UVM_RM_RECORD = Value
  }

  type uvmAction = Seq[ENUM_UVM_ACTION.Value]
}
