package uvm

abstract class UVMComponent(name: String, parent: Option[UVMComponent] = None) extends UVMReportObject(name) {
  def getParent: UVMComponent = parent match {
    case Some(value) => value
    case None => UVMCoreService().getRoot
  }
}
