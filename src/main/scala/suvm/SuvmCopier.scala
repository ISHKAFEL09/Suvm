package suvm

class SuvmCopier(name: String) extends SuvmPolicy(name) {
  def copyObject(lhs: SuvmObject, rhs: SuvmObject): Option[SuvmObject]

  override def createObj(name: String): Option[SuvmObject] = ???

  override def doRecord(recorder: SuvmRecorder): Unit = ???

  override def doCopy(rhs: SuvmObject): Unit = ???

  override def doCompare(rhs: SuvmObject, comparer: SuvmComparer): Boolean = ???

  override def doPack(packer: SuvmPacker): Unit = ???

  override def doUnpack(packer: SuvmPacker): Unit = ???

  override def doExecuteOp(op: SuvmFieldOp): Unit = ???
}
