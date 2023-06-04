package uvm

import tlm._

class UVMAnalysisPort[T](name: String, parent: Option[UVMComponent])
  extends UVMPortBase[T](name, parent, ENUM_PORT_TYPE.UVM_PORT)
    with UVMTlmIfBase[T] {

  override def getTypeName: String = "UVMAnalysisPort"

  override def write(t: T): Unit = {
    getAllIF.foreach(_.asInstanceOf[UVMTlmIfBase[T]].write(t))
  }
}

class UVMAnalysisExport[T](name: String, parent: Option[UVMComponent])
  extends UVMPortBase[T](name, parent, ENUM_PORT_TYPE.UVM_EXPORT)
    with UVMTlmIfBase[T] {

  override def getTypeName: String = "UVMAnalysisExport"

  override def write(t: T): Unit = {
    getAllIF.foreach(_.asInstanceOf[UVMTlmIfBase[T]].write(t))
  }
}

class UVMAnalysisImp[T](name: String, f: T => Unit)
  extends UVMPortBase[T](name, None, ENUM_PORT_TYPE.UVM_IMPLEMENTATION)
    with UVMTlmIfBase[T] {

  override def getTypeName: String = "UVMAnalysisImp"

  override def write(t: T): Unit = f(t)
}
