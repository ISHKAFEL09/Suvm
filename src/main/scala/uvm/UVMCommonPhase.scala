package uvm

import ENUM_PHASE_TYPE._
import ENUM_PHASE_STATE._

abstract class UVMTopDownPhase(name: String) extends UVMPhase(name, UVM_PHASE_IMP) {
  override def traverse(comp: UVMComponent, phase: UVMPhase, state: uvmPhaseState): Unit = {
    state match {
      case UVM_PHASE_STARTED =>
      // TODO:
      case UVM_PHASE_EXECUTING =>
        execute(comp, phase)
      case UVM_PHASE_READY_TO_END =>
      // TODO:
      case UVM_PHASE_ENDED =>
        comp.phaseEnded(phase)
      // TODO:
      case _ =>
        uvmFatal("PH_BADEXEC","topdown phase traverse internal error")
    }
    comp.getChildren.foreach(traverse(_, phase, state))
  }

  override def execute(comp: UVMComponent, phase: UVMPhase): Unit = {
    execFunc(comp, phase)
  }
}

abstract class UVMBottomUpPhase(name: String) extends UVMPhase(name, UVM_PHASE_IMP) {
  override def traverse(comp: UVMComponent, phase: UVMPhase, state: uvmPhaseState): Unit = {
    comp.getChildren.foreach(traverse(_, phase, state))
    state match {
      case UVM_PHASE_STARTED =>
      // TODO:
      case UVM_PHASE_EXECUTING =>
        execute(comp, phase)
      case UVM_PHASE_READY_TO_END =>
      // TODO:
      case UVM_PHASE_ENDED =>
        comp.phaseEnded(phase)
      // TODO:
      case _ =>
        uvmFatal("PH_BADEXEC","topdown phase traverse internal error")
    }
  }

  override def execute(comp: UVMComponent, phase: UVMPhase): Unit = {
    execFunc(comp, phase)
  }
}

abstract class UVMTaskPhase(name: String) extends UVMBottomUpPhase(name) {
  override def execute(comp: UVMComponent, phase: UVMPhase): Unit = {
    fork(s"${phase.getName}_${comp.getName}_execute") {
      execTask(comp, phase)
    }
  }
}

class UVMBuildPhase(name: String = "build") extends UVMTopDownPhase(name) {
  override def execFunc(comp: UVMComponent, phase: UVMPhase): Unit = {
    comp.buildPhase(phase)
  }
}

class UVMConnectPhase(name: String = "connect") extends UVMBottomUpPhase(name) {
  override def execFunc(comp: UVMComponent, phase: UVMPhase): Unit = {
    comp.connectPhase(phase)
  }
}

class UVMRunPhase(name: String = "run") extends UVMTaskPhase(name) {
  override def execTask(comp: UVMComponent, phase: UVMPhase): Unit = {
    comp.runPhase(phase)
  }
}

class UVMExtractPhase(name: String = "extract") extends UVMBottomUpPhase(name) {
  override def execFunc(comp: UVMComponent, phase: UVMPhase): Unit = {
    comp.extractPhase(phase)
  }
}

class UVMCheckPhase(name: String = "check") extends UVMBottomUpPhase(name) {
  override def execFunc(comp: UVMComponent, phase: UVMPhase): Unit = {
    comp.checkPhase(phase)
  }
}

class UVMReportPhase(name: String = "report") extends UVMBottomUpPhase(name) {
  override def execFunc(comp: UVMComponent, phase: UVMPhase): Unit = {
    comp.reportPhase(phase)
  }
}

class UVMFinalPhase(name: String = "final") extends UVMBottomUpPhase(name) {
  override def execFunc(comp: UVMComponent, phase: UVMPhase): Unit = {
    comp.finalPhase(phase)
  }
}

object UVMCommonPhase {
  private lazy val buildPhase = new UVMBuildPhase()
  private lazy val connectPhase = new UVMConnectPhase()
  private lazy val runPhase = new UVMRunPhase()
  private lazy val extractPhase = new UVMExtractPhase()
  private lazy val checkPhase = new UVMCheckPhase()
  private lazy val reportPhase = new UVMReportPhase()
  private lazy val finalPhase = new UVMFinalPhase()
  def apply(name: String): UVMPhase = name match {
    case "build" => buildPhase
    case "connect" => connectPhase
    case "run" => runPhase
    case "extract" => extractPhase
    case "check" => checkPhase
    case "report" => reportPhase
    case "final" => finalPhase
    case _ =>
      uvmFatal("CPGET", s"No common phase named $name")
      throw new IllegalArgumentException()
  }
}
