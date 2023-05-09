package uvm

trait UVMVoid

abstract class UVMObject(val name: String) extends UVMVoid {
  private val mInstID: Int = {
    UVMObject.mInstCount += 1
    UVMObject.mInstCount
  }

  private val mLeafName: String = name

  def getUVMSeeding: Boolean = UVMCoreService().getUVMSeeding

  def setUVMSeeding(enable: Boolean): Unit = UVMCoreService().setUVMSeeding(enable)


}

object UVMObject {
  private var mInstCount: Int = 0
}

