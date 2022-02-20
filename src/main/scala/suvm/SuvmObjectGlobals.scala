package suvm

object SuvmObjectGlobals {
  object SuvmRadixEnum extends Enumeration {
    val UVM_BIN = Value
    val UVM_DEC = Value
    val UVM_UNSIGNED = Value
    val UVM_UNFORMAT2 = Value
    val UVM_UNFORMAT4 = Value
    val UVM_OCT = Value
    val UVM_HEX = Value
    val UVM_STRING = Value
    val UVM_TIME = Value
    val UVM_ENUM = Value
    val UVM_REAL = Value
    val UVM_REAL_DEC = Value
    val UVM_REAL_EXP = Value
    val UVM_NORADIX = Value
  }

  object SuvmOpcodeEnum extends Enumeration {
    val UVM_COPY = Value
    val UVM_NOCOPY = Value
    val UVM_COMPARE = Value
    val UVM_NOCOMPARE = Value
    val UVM_PRINT = Value
    val UVM_NOPRINT = Value
    val UVM_RECORD = Value
    val UVM_NORECORD = Value
    val UVM_PACK = Value
    val UVM_NOPACK = Value
    val UVM_UNPACK = Value
    val UVM_NOUNPACK = Value
    val UVM_SET = Value
    val UVM_NOSET = Value
    val UVM_NODEFPRINT = Value
    val UVM_FLAGS_ON = Value
    val UVM_FLAGS_OFF = Value
    val UVM_ALL_ON = UVM_FLAGS_ON
    val UVM_DEFAULT = UVM_ALL_ON
  }

  object SuvmVerbosity extends Enumeration {
    val UVM_NONE = Value(0)
    val UVM_LOW = Value(100)
    val UVM_MEDIUM = Value(200)
    val UVM_HIGH = Value(300)
    val UVM_FULL = Value(400)
    val UVM_DEBUG = Value(500)
  }

  object SuvmActionType extends Enumeration {
    val UVM_NO_ACTION = Value(0x0)
    val UVM_DISPLAY   = Value(0x1)
    val UVM_LOG       = Value(0x2)
    val UVM_COUNT     = Value(0x4)
    val UVM_EXIT      = Value(0x8)
    val UVM_CALL_HOOK = Value(0x10)
    val UVM_STOP      = Value(0x12)
    val UVM_RM_RECORD = Value(0x14)
  }

  object SuvmCoreState extends Enumeration {
    val UVM_CORE_UNINITIALIZED = Value
    val UVM_CORE_PRE_INIT = Value
    val UVM_CORE_INITIALIZING = Value
    val UVM_CORE_INITIALIZED = Value
    val UVM_CORE_PRE_RUN = Value
    val UVM_CORE_RUNNING = Value
    val UVM_CORE_POST_RUN = Value
    val UVM_CORE_FINISHED = Value
    val UVM_CORE_PRE_ABORT = Value
    val UVM_CORE_ABORTED = Value
    val UVM_CORE_POST_INIT = UVM_CORE_INITIALIZED
  }
  var mSuvmCoreState = SuvmCoreState.UVM_CORE_UNINITIALIZED

  object SuvmSeverity extends Enumeration {
    val UVM_INFO = Value
    val UVM_WARNING = Value
    val UVM_ERROR = Value
    val UVM_FATAL = Value
  }

  def SuvmReportError(id: String, msg: String, verbosity: SuvmVerbosity.Value = SuvmVerbosity.UVM_NONE,
                      fileName: String = "", line: Int = 0, contextName: String = "",
                      reportEnabledChecked: Boolean = false): Unit = {
    // TODO
  }

  def suvmReportEnabled(verbosity: SuvmVerbosity.Value,
                        severity: SuvmSeverity.Value,
                        id: String): Boolean = ???

  def suvmReportInfo(id: String,
                     msg: String,
                     verbosity: SuvmVerbosity.Value,
                     filename: String,
                     line: Int,
                     contextName: String,
                     enabled: Boolean): Unit = ???

  def suvmInfo(id: String, msg: String, verbosity: SuvmVerbosity.Value): Unit =
    SuvmMessage.suvmInfo(id, msg, verbosity, suvmReportEnabled, suvmReportInfo)

  val UVM_STDOUT: String = "SuvmReport.log"
}
