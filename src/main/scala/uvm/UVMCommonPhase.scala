package uvm

import ENUM_PHASE_TYPE._

abstract class UVMTopdownPhase(name: String) extends UVMPhase(name, UVM_PHASE_IMP) {
  def traverse(comp: UVMComponent, phase: UVMPhase, state: uvmPhaseState): Unit = {

  }
}