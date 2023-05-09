package object uvm {
  // private, for uvm
  private[uvm] def getTrace(level: Int = 2): String = {
    new Throwable().getStackTrace()(level).toString
  }

  private[uvm] def uvmInit(cs: Option[UVMCoreService] = None): Unit = {
    // TODO:
    UVMCoreService.set(new UVMDefaultCoreService)
    UVMRoot().uvmFatal("Init", "this it uvmInit")
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
}
