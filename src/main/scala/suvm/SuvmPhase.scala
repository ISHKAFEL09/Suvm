package suvm

import SuvmObjectGlobals._

class SuvmPhase(val name: String) extends SuvmObject {
  def getState: SuvmPhaseState.Value = mState

  def setState(s: SuvmPhaseState.Value): Unit = mState = s

  private var mState = SuvmPhaseState.UVM_PHASE_UNINITIALIZED
}
