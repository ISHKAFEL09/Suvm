package uvm

trait UVMVoid

abstract class UVMObject(name: String) extends UVMVoid {
  private val mInstID: Int = {
    UVMObject.mInstCount += 1
    UVMObject.mInstCount
  }

  private var mLeafName: String = name

  def getUVMSeeding: Boolean = UVMCoreService().getUVMSeeding

  def setUVMSeeding(enable: Boolean): Unit = UVMCoreService().setUVMSeeding(enable)

  def setName(name: String): Unit = mLeafName = name

  def getName: String = mLeafName

  def getFullName: String = getName

  def getTypeName: String = getClass.getPureName

  def getInstID: Int = mInstID

  override def toString: String = getFullName
}

object UVMObject {
  var mInstCount: Int = 0

  def getInstCount: Int = mInstCount
}
