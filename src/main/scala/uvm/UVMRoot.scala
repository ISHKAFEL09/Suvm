package uvm

import chiter._

import scala.reflect.ClassTag

class UVMRoot extends UVMComponent("__top__") {
  override val _traceLevel: Int = 4

  var mPhaseAllDone = false

  override def phaseEnded(phase: UVMPhase): Unit = {
    if (phase.toString == "connect") {
      doResolveBindings()
    }
  }
}

object UVMRoot {
  private lazy val mInst: UVMRoot = new UVMRoot

  private[uvm] def apply(): UVMRoot = mInst
}