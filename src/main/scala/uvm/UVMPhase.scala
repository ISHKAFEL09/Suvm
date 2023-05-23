package uvm

import chiseltester._
import ENUM_PHASE_TYPE._
import ENUM_PHASE_STATE._
import uvm.UVMPhase.mExecutionPhases

class UVMPhase(name: String = "uvmPhase",
               phaseType: uvmPhaseType = UVM_PHASE_SCHEDULE,
               parent: Option[UVMPhase] = None) extends UVMObject(name) {

  private var mRunCount: Int = 0

  private val mPhaseType: uvmPhaseType = phaseType

  private var mState: uvmPhaseState =
    if (name == "common" && phaseType == UVM_PHASE_DOMAIN)
      UVM_PHASE_DORMANT
    else
      UVM_PHASE_UNINITIALIZED

  private val mParent: Option[UVMPhase] = parent

  private var mPhaseProc: Option[TesterThreadList] = None
  private var phaseDone: Option[UVMObjection] = None

  // TODO: cmd line args

  private var mEndNode: Option[UVMPhase] = None
  private val mSuccessors = collection.mutable.ListBuffer.empty[UVMPhase]
  private val mPredecessors = collection.mutable.ListBuffer.empty[UVMPhase]
  if (parent.isEmpty && Seq(UVM_PHASE_SCHEDULE, UVM_PHASE_DOMAIN).contains(phaseType)) {
    mEndNode = Some(new UVMPhase(s"${name}_end", UVM_PHASE_TERMINAL, Some(this)))
    mSuccessors += mEndNode.get
    mEndNode.get.mPredecessors += this
  }

  def getPhaseType: uvmPhaseType = mPhaseType

  private var mImp: Option[UVMPhase] = None

  def add(phase: UVMPhase): Unit = {
    phase.getPhaseType match {
      case UVM_PHASE_IMP =>
        val newNode = new UVMPhase(phase.getName, UVM_PHASE_NODE, Some(this))
        newNode.mImp = Some(phase)
        require(mEndNode.nonEmpty)
        mEndNode.get.mPredecessors.foreach { i =>
          i.mSuccessors -= mEndNode.get
          i.mSuccessors += newNode
        }
        newNode.mPredecessors ++= mEndNode.get.mPredecessors
        mEndNode.get.mPredecessors.clear()
        mEndNode.get.mPredecessors += newNode
        newNode.mSuccessors += mEndNode.get
        newNode.mState = UVM_PHASE_DORMANT
      case _ =>
    }
  }

  def traverse(comp: UVMComponent, phase: UVMPhase, state: uvmPhaseState): Unit = {}

  def execute(comp: UVMComponent, phase: UVMPhase): Unit = {}

  def execFunc(comp: UVMComponent, phase: UVMPhase): Unit = {}

  def execTask(comp: UVMComponent, phase: UVMPhase): Unit = {}

  def getObjection: Option[UVMObjection] = {
    if (mPhaseType != UVM_PHASE_NODE || mImp.isEmpty)
      None
    else if (phaseDone.isEmpty) {
      phaseDone = Some(create(s"${getName}_objection") { s =>
        new UVMObjection(s)
      })
      phaseDone
    } else
      phaseDone
  }

  def executePhase(): Unit = {
    mPhaseType match {
      case UVM_PHASE_NODE =>
        mState = UVM_PHASE_STARTED
        mImp.get.traverse(top, this, UVM_PHASE_STARTED)
        ~>(0)
        mImp.get match {
          case taskPhase: UVMTaskPhase =>
            mExecutionPhases += this
            mState = UVM_PHASE_EXECUTING
            mPhaseProc = Some(fork(s"phase_${getName}_executing") {
              taskPhase.traverse(top, this, UVM_PHASE_EXECUTING)
              ~>(false)
            })
            ~>(0)
          case _ =>
            mState = UVM_PHASE_EXECUTING
            ~>(0)
            mImp.get.traverse(top, this, UVM_PHASE_EXECUTING)
        }
      case _ =>
        mState = UVM_PHASE_STARTED
        ~>(0)
        mState = UVM_PHASE_EXECUTING
    }
  }
}

object UVMPhase {
  private val mPhaseHopper = collection.mutable.Queue.empty[UVMPhase]
  private val mExecutionPhases = collection.mutable.Queue.empty[UVMPhase]

  def mRunPhases(): Unit = {
    mPhaseHopper += UVMDomain.getCommonDomain

    @annotation.tailrec
    def phaseLoop(): Unit = {
      ~> (mPhaseHopper.nonEmpty)
      val phase = mPhaseHopper.dequeue()
      fork(s"phase_${phase.getName}") {
        phase.executePhase()
      }
      ~> (0)
      phaseLoop()
    }

    phaseLoop()
  }
}
