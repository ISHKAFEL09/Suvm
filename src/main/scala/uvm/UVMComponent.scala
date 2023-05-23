package uvm

abstract class UVMComponent(name: String, parent: Option[UVMComponent] = None) extends UVMReportObject(name) {
  private var mName: String = ""
  private var mParent: Option[UVMComponent] = None
  private var mChildren = collection.mutable.ListBuffer.empty[UVMComponent]
  private var mChildrenByName = collection.mutable.HashMap.empty[String, UVMComponent]

  if (parent.isEmpty && name == "__top__") { // this is uvmRoot
    setName("")
  } else {
    // TODO: check phase
    mParent = if (parent.isEmpty) Some(top) else parent
    setName(if(name.isEmpty) s"COMP_${UVMObject.mInstCount}" else name)
    if (!mParent.get.mAddChild(this))
      mParent = None
    mReportHandler.setName(getFullName)
    setReportVerbosityLevel(mParent.getOrElse(top).getReportVerbosityLevel())
    // TODO: cmd line args
  }

  override def setName(name: String): Unit = {
    if (mName.nonEmpty) {
      uvmError("INVSTNM", "It's illegal to change the name of a component.")
    } else {
      super.setName(name)
      mSetFullName()
    }
  }

  private def mSetFullName(): Unit = {
    if (mParent.isEmpty || mParent.get == top) {
      mName = getName
    } else {
      mName = s"${mParent.get.getFullName}.$getName"
    }
    mChildren.foreach(_.mSetFullName())
  }

  override def getFullName: String = {
    if (mName.isEmpty) getName
    else mName
  }

  def getParent: UVMComponent = parent match {
    case Some(value) => value
    case None => top
  }

  def getChildren: List[UVMComponent] = mChildren.toList

  private def mAddChild(child: UVMComponent): Boolean = {
    if (mChildrenByName.contains(child.getName) && mChildrenByName(child.getName) != child) {
      uvmWarning("BDCLD", s"A child with the name ${child.getName} already exists.")
      false
    } else if (mChildren.contains(child)) {
      uvmWarning("BDCHID", s"A child with the name ${child.getName} already exists.")
      false
    } else {
      mChildren += child
      mChildrenByName(child.getName) = child
      true
    }
  }

  def buildPhase(phase: UVMPhase): Unit = {}
  def connectPhase(phase: UVMPhase): Unit = {}
  def runPhase(phase: UVMPhase): Unit = {}
  def extractPhase(phase: UVMPhase): Unit = {}
  def checkPhase(phase: UVMPhase): Unit = {}
  def reportPhase(phase: UVMPhase): Unit = {}
  def finalPhase(phase: UVMPhase): Unit = {}
}
