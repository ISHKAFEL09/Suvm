package suvm

import SuvmImplicits._

object SuvmObjectGlobals {
  object SuvmRadix extends Enumeration {
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
    val UVM_COPY = Value(1 << 0)
    val UVM_NOCOPY = Value(1 << 1)
    val UVM_COMPARE = Value(1 << 2)
    val UVM_NOCOMPARE = Value(1 << 3)
    val UVM_PRINT = Value(1 << 4)
    val UVM_NOPRINT = Value(1 << 5)
    val UVM_RECORD = Value(1 << 6)
    val UVM_NORECORD = Value(1 << 7)
    val UVM_PACK = Value(1 << 8)
    val UVM_NOPACK = Value(1 << 9)
    val UVM_UNPACK = Value(1 << 10)
    val UVM_NOUNPACK = UVM_NOPACK
    val UVM_SET = Value(1 << 11)
    val UVM_NOSET = Value(1 << 12)
    val UVM_NODEFPRINT = Value(1 << 15)
    val UVM_FLAGS_ON = Value(UVM_COPY.id | UVM_COMPARE.id | UVM_PRINT.id |
                             UVM_RECORD.id | UVM_PACK.id | UVM_UNPACK.id | UVM_SET.id)
    val UVM_FLAGS_OFF = Value(0)
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
    def find(id: Int): Option[Value] = values.find(_.id == id)
  }

  object SuvmAction extends Enumeration {
    val UVM_NO_ACTION = Value(0x0)
    val UVM_DISPLAY   = Value(0x1)
    val UVM_LOG       = Value(0x2)
    val UVM_COUNT     = Value(0x4)
    val UVM_EXIT      = Value(0x8)
    val UVM_CALL_HOOK = Value(0x10)
    val UVM_STOP      = Value(0x20)
    val UVM_RM_RECORD = Value(0x40)
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

  object SuvmPhaseState extends Enumeration {
    val UVM_PHASE_UNINITIALIZED = Value(0)
    val UVM_PHASE_DORMANT = Value(1)
    val UVM_PHASE_SCHEDULED = Value(2)
    val UVM_PHASE_SYNCING = Value(4)
    val UVM_PHASE_STARTED = Value(8)
    val UVM_PHASE_EXECUTING = Value(16)
    val UVM_PHASE_READY_TO_END = Value(32)
    val UVM_PHASE_ENDED = Value(64)
    val UVM_PHASE_CLEANUP = Value(128)
    val UVM_PHASE_DONE = Value(256)
    val UVM_PHASE_JUMPING = Value(512)
  }

  object SuvmPortType extends Enumeration {
    val UVM_PORT, UVM_EXPORT, UVM_IMPLEMENTATION = Value
  }

  object SuvmRecursionPolicy extends Enumeration {
    val UVM_DEFAULT_POLICY = Value(0)
    val UVM_DEEP = Value(1 << 16)
    val UVM_SHALLOW = Value(1 << 17)
    val UVM_REFERENCE = Value(1 << 18)
  }

  def suvmReportError(id: String, msg: String, verbosity: Int = SuvmVerbosity.UVM_NONE,
                      fileName: String = "", line: Int = 0, contextName: String = "",
                      reportEnabledChecked: Boolean = false): Unit = {
    println(s"[$id] $msg")
  }

  def suvmReportEnabled(verbosity: Int,
                        severity: SuvmSeverity.Value,
                        id: String): Boolean = true

  def suvmReportInfo(id: String,
                     msg: String,
                     verbosity: Int = SuvmVerbosity.UVM_MEDIUM,
                     filename: String = "",
                     line: Int = 0,
                     contextName: String = "",
                     enabled: Boolean = false): Unit = {
    println(s"$id: $msg")
  }

  def suvmReportFatal(id: String,
                     msg: String,
                     verbosity: SuvmVerbosity.Value = SuvmVerbosity.UVM_NONE,
                     filename: String = "",
                     line: Int = 0,
                     contextName: String = "",
                     enabled: Boolean = false): Unit = ???

  def suvmReportWarning(id: String,
                        msg: String,
                        verbosity: Int,
                        filename: String,
                        line: Int,
                        contextName: String,
                        enabled: Boolean): Unit = ???

  def suvmWarning(id: String, msg: String): Unit =
    SuvmMessage.suvmWarning(id, msg, suvmReportEnabled, suvmReportWarning)

  def suvmInfo(id: String, msg: String, verbosity: SuvmVerbosity.Value): Unit =
    SuvmMessage.suvmInfo(id, msg, verbosity, suvmReportEnabled, suvmReportInfo)

  def suvmError(id: String, msg: String): Unit =
    SuvmMessage.suvmError(id, msg, suvmReportEnabled, suvmReportError)

  val UVM_STDOUT: String = "SuvmReport.log"
}
