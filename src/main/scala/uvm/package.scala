import scala.language.implicitConversions
import scala.reflect.ClassTag
import spinal.core._
import spinal.core.sim._
import spinal.sim._

package object uvm {
  // private, for uvm
  private[uvm] def getTrace(level: Int = 2): String = {
    new Throwable().getStackTrace()(level).toString
  }

  private[uvm] def uvmInit(cs: Option[UVMCoreService] = None): Unit = {
    // TODO:
    UVMCoreService.set(new UVMDefaultCoreService)
  }

  implicit class classOps[T](c: Class[T]) {
    def getPureName: String = c.getSimpleName.replaceAll("\\$.+?$", "")
  }

  implicit def class2string[T](c: Class[T]): String = c.getPureName

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

  lazy val top: UVMRoot = UVMCoreService().getRoot

  lazy val uvmReportEnabled: (uvmVerbosity, uvmSeverity, String) => Boolean =
    top.uvmReportEnabled

  lazy val uvmReport: (uvmSeverity, String, String, uvmVerbosity, String, String, Boolean) => Unit =
    top.uvmReport

  lazy val uvmReportInfo: (String, String, uvmVerbosity, String, String, Boolean) => Unit =
    top.uvmReportInfo

  lazy val uvmReportWarning: (String, String, uvmVerbosity, String, String, Boolean) => Unit =
    top.uvmReportWarning

  lazy val uvmReportFatal: (String, String, uvmVerbosity, String, String, Boolean) => Unit =
    top.uvmReportFatal

  lazy val uvmInfo: (String, String, uvmVerbosity) => Unit =
    top.uvmInfo

  lazy val uvmWarning: (String, String) => Unit =
    top.uvmWarning

  lazy val uvmFatal: (String, String) => Unit =
    top.uvmFatal

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

  object ENUM_PHASE_TYPE extends Enumeration {
    val UVM_PHASE_IMP = Value
    val UVM_PHASE_NODE = Value
    val UVM_PHASE_TERMINAL = Value
    val UVM_PHASE_SCHEDULE = Value
    val UVM_PHASE_DOMAIN = Value
    val UVM_PHASE_GLOBAL = Value
  }

  type uvmPhaseType = ENUM_PHASE_TYPE.Value

  object ENUM_PHASE_STATE extends Enumeration {
    val UVM_PHASE_UNINITIALIZED = Value
    val UVM_PHASE_DORMANT = Value
    val UVM_PHASE_SCHEDULED = Value
    val UVM_PHASE_SYNCING = Value
    val UVM_PHASE_STARTED = Value
    val UVM_PHASE_EXECUTING = Value
    val UVM_PHASE_READY_TO_END = Value
    val UVM_PHASE_ENDED = Value
    val UVM_PHASE_CLEANUP = Value
    val UVM_PHASE_DONE = Value
    val UVM_PHASE_JUMPING = Value
  }

  type uvmPhaseState = ENUM_PHASE_STATE.Value

  object ENUM_OBJECTION_EVENT extends Enumeration {
    val UVM_RAISED = Value
    val UVM_DROPPED = Value
    val UVM_ALL_DROPPED = Value
  }

  type uvmObjectionEvent = ENUM_OBJECTION_EVENT.Value

  object ENUM_PORT_TYPE extends Enumeration {
    val UVM_PORT = Value
    val UVM_EXPORT = Value
    val UVM_IMPLEMENTATION = Value
  }

  type uvmPortType = ENUM_PORT_TYPE.Value

  object ENUM_ACTIVE_PASSIVE extends Enumeration {
    val UVM_PASSIVE = Value
    val UVM_Active = Value
  }

  def create[T <: UVMObject : ClassTag](name: String)(f: String => T): T = {
    val objName = implicitly[ClassTag[T]].runtimeClass.getPureName
    val factory = UVMCoreService().getFactory
    val wrapper = factory.getWrapperByName[T](objName)
    wrapper match {
      case Some(wrapper) =>
        factory.createObjectByType(wrapper, name)
      case None =>
        val w = new UVMObjectRegistry(objName, f)
        factory.register(w)
        factory.createObjectByType(w, name)
    }
  }

  def create[T <: UVMComponent : ClassTag](name: String, parent: UVMComponent)(
    f: (String, UVMComponent) => T): T = {
    val objName = implicitly[ClassTag[T]].runtimeClass.getPureName
    val factory = UVMCoreService().getFactory
    val wrapper = factory.getWrapperByName[T](objName)
    wrapper match {
      case Some(wrapper) =>
        factory.createComponentByType(wrapper, name, Some(parent))
      case None =>
        val w = new UVMComponentRegistry(objName, (s: String, p: Option[UVMComponent]) => f(s, p.get))
        factory.register(w)
        factory.createComponentByType(w, name, Some(parent))
    }
  }

  trait HarnessBase extends Component {
    def init(): Unit
  }

  def uvmRun[T <: UVMTest : ClassTag, H <: HarnessBase](harness: => H,
                                                        simCfg: SpinalSimConfig = SpinalSimConfig().withWave)(tc: H => T): Unit = {
    val tcName = implicitly[ClassTag[T]].runtimeClass.getPureName
    val cmp = simCfg.compile(harness)
    cmp.doSim(tcName) { h =>
      h.init()
      create(tcName, top) { case (_, _) => tc(h) }
      val runner = fork(tcName) {
        UVMPhase.mRunPhases()
      }
      ~>(0)
      ~>(top.mPhaseAllDone)
      runner.terminate()
      finish(SuccessStatus)
    }
  }

  private var forkID: Int = 0
  private def getForkID: Int = synchronized {
    forkID += 1
    forkID
  }

  def fork(name: String = "thread")(runnable: => Unit): SimThread = sim.fork(runnable)

  def fork(runnable: => Unit): SimThread = fork(s"$getForkID")(runnable)

  def ~>(condition: => Boolean): Unit = sim.waitUntil(condition)

  def ~>(cycles: Int): Unit = {
    if (cycles == 0) sim.fork(sim.sleep(0)).join()
    sim.sleep(cycles)
  }

  def ~>(event: UVMEvent): Unit = sim.waitUntil(event.isTriggered)

  def time(): BigInt = sim.simTime()

  def createEvent(name: String): UVMEvent = UVMEvent(name)

  def getCurrent: SimThread = SimManagerContext.current.thread

  sealed trait FinishStatus

  object SuccessStatus extends FinishStatus

  object StopStatus extends FinishStatus

  object FatalStatus extends FinishStatus

  def finish(status: FinishStatus): Unit = {
    val t = time()
    status match {
      case SuccessStatus =>
        println(s"simulation finished @$t")
        sim.simSuccess()
      case StopStatus =>
        sim.simFailure(s"finish() called in ${new Throwable().getStackTrace()(4).toString}")
      case FatalStatus =>
        sim.simFailure("Fatal!!!")
    }
  }

  def debugLog(s: String): Unit = {
//    println(s"[DEBUG LOG] $s")
  }
}
