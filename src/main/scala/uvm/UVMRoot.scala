package uvm

import chiseltester._

import scala.reflect.ClassTag

class UVMRoot extends UVMComponent("__top__") {
  override val _traceLevel: Int = 4

  var mPhaseAllDone = false
}

object UVMRoot {
  private lazy val mInst: UVMRoot = new UVMRoot

  private[uvm] def apply(): UVMRoot = mInst
}