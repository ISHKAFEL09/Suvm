package suvm

import SuvmObjectGlobals._

sealed abstract class SuvmTopdownPhase(name: String) extends SuvmPhase(name) {
  def traverse(comp: SuvmComponent, phase: SuvmPhase, state: SuvmPhaseState.Value): Unit =
    ???
  def execute(comp: SuvmComponent, phase: SuvmPhase): Unit =
    ???
}

sealed abstract class SuvmBottomupPhase(name: String) extends SuvmPhase(name) {
  def traverse(comp: SuvmComponent, phase: SuvmPhase, state: SuvmPhaseState.Value): Unit =
    ???
  def execute(comp: SuvmComponent, phase: SuvmPhase): Unit =
    ???
}

sealed abstract class SuvmTaskPhase(name: String) extends SuvmPhase(name) {

}

object SuvmBuildPhase extends SuvmTopdownPhase("build")
object SuvmFinalPhase extends SuvmTopdownPhase("final")

object SuvmConnectPhase extends SuvmBottomupPhase("connect")
object SuvmEndOfElaborationPhase extends SuvmBottomupPhase("endOfElaboration")
object SuvmStartOfSimulationPhase extends SuvmBottomupPhase("startOfSimulation")
object SuvmExtractPhase extends SuvmBottomupPhase("extract")
object SuvmCheckPhase extends SuvmBottomupPhase("check")
object SuvmReportPhase extends SuvmBottomupPhase("report")

object SuvmRunPhase extends SuvmTaskPhase("run")