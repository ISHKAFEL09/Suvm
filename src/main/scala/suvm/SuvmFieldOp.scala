package suvm

class SuvmFieldOp(name: String) extends SuvmObject(name) {
  def set(opType: SuvmOpcodeEnum.Value, policy: Option[SuvmPolicy] = None, rhs: Option[SuvmObject] = None): Unit
  def flush(): Unit
  def mRecycle(): Unit = {
    flush()
    SuvmFieldOp.mRecycledOp.enqueue(this)
  }
}

object SuvmFieldOp extends {
  private val mRecycledOp = scala.collection.mutable.Queue.empty[SuvmFieldOp]

  def getAvailableOp: SuvmFieldOp = {
    if (mRecycledOp.nonEmpty) mRecycledOp.dequeue()
    else new SuvmFieldOp("FieldOp")
  }
}