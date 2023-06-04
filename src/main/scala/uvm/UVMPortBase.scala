package uvm

import chiseltester.finish
import uvm.tlm.UVMTlmIfBase


abstract class UVMPortComponentBase(name: String, parent: Option[UVMComponent]) extends UVMComponent(name, parent) {
  type thisType

  def getConnected: Map[String, thisType]

  def isPort: Boolean

  def isExport: Boolean

  def isImp: Boolean
}

abstract class UVMPortBase[IF](name: String,
                               parent: Option[UVMComponent],
                               portType: uvmPortType,
                               minSize: Int = 0,
                               maxSize: Int = 1) extends UVMPortComponentBase(name, parent) {
  this: UVMTlmIfBase[IF] =>

  override type thisType = UVMPortBase[IF]

  private val mProvidedBy = collection.mutable.HashMap.empty[String, thisType]
  private val mProvidedTo = collection.mutable.HashMap.empty[String, thisType]
  private val mImpList = collection.mutable.HashMap.empty[String, thisType]
  private var mResolved = false

  override def getTypeName: String = portType match {
    case ENUM_PORT_TYPE.UVM_PORT => "port"
    case ENUM_PORT_TYPE.UVM_EXPORT => "export"
    case ENUM_PORT_TYPE.UVM_IMPLEMENTATION => "implementation"
  }

  override def isPort: Boolean = portType == ENUM_PORT_TYPE.UVM_PORT

  override def isExport: Boolean = portType == ENUM_PORT_TYPE.UVM_EXPORT

  override def isImp: Boolean = portType == ENUM_PORT_TYPE.UVM_IMPLEMENTATION

  def connect(provider: thisType): Unit = {
    UVMCommonPhase("connect").getState match {
      case ENUM_PHASE_STATE.UVM_PHASE_DONE =>
        uvmWarning("Late Connection", s"Attempt to connect $getFullName " +
          s"(of type $getTypeName) after connect phase. Ignoring.")
      case _ =>
        assert(provider != this)
        if (isImp)
          uvmError("Connection Error", "Cannot call an imp port's connect method")
        else if (isExport && provider.isPort)
          uvmError("Connection Error", "Cannot connect exports to ports")
        else {
          mProvidedBy(provider.getFullName) = provider
          provider.mProvidedTo(getFullName) = this
        }
    }
  }

  override def getConnected: Map[String, thisType] = {
    mProvidedBy.toMap
  }

  def getProvided: Map[String, thisType] = {
    mProvidedTo.toMap
  }

  def size: Int = mImpList.size

  def getIF(index: Int = 0): thisType = getAllIF(index)

  def getAllIF: List[thisType] = mImpList.values.toList

  private def mAddList(provider: thisType): Unit =
    provider.getAllIF.foreach { imp =>
      mImpList.getOrElseUpdate(imp.getFullName, imp)
    }

  override def resolveBindings(): Unit = {
    if (!mResolved) {
      if (isImp)
        mImpList(getFullName) = this
      else {
        mProvidedBy.foreach { case (_, port) =>
          port.resolveBindings()
          mAddList(port)
        }
      }
    }
    mResolved = true
  }
}
