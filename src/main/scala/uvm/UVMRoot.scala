package uvm

class UVMRoot extends UVMComponent("__top__") {
  override val _traceLevel: Int = 4
}

object UVMRoot {
  private var mInst: Option[UVMRoot] = None

  private[uvm] def apply(): UVMRoot = {
    mInst match {
      case Some(uvmRoot) => uvmRoot
      case None =>
        val top = new UVMRoot
        mInst = Some(top)
        top
    }
  }
}