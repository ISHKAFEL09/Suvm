package uvm


import uvm.UVMObjection.mObjections

case class UVMObjectionEvents(var waiters: Int,
                              raised: UVMEvent,
                              dropped: UVMEvent,
                              allDropped: UVMEvent)

class UVMObjection(name: String = "") extends UVMReportObject(name) {
  private val mSourceCount = collection.mutable.HashMap.empty[UVMObject, Int]
  private val mTotalCount = collection.mutable.HashMap.empty[UVMObject, Int]
  private val mEvents = collection.mutable.HashMap.empty[UVMObject, UVMObjectionEvents]
  private var mTopAllDropped = false

  private def mGetParent(obj: UVMObject): UVMObject = obj match {
    case comp: UVMComponent => comp.getParent
    case _ => top
  }

  private def mProgagate(obj: UVMObject,
                         srcObj: UVMObject,
                         description: String,
                         count: Int,
                         raise: Boolean): Unit = {
    if (obj != top) {
      if (raise) mRaise(mGetParent(obj), srcObj, description, count)
      else mDrop(mGetParent(obj), srcObj, description, count)
    }
  }

  private def mRaise(obj: UVMObject,
                     srcObj: UVMObject,
                     description: String,
                     count: Int = 1): Unit = {
    require(count > 0)

    mTotalCount.update(obj, mTotalCount.getOrElse(obj, 0) + count)

    if (obj == srcObj) {
      mSourceCount.update(obj, mSourceCount.getOrElse(obj, 0) + count)
    }

    // TODO: raised
    if (mEvents.contains(obj))
      mEvents(obj).raised.trigger()

    mProgagate(obj, srcObj, description, count, raise = true)
  }

  def raiseObjection(obj: Option[UVMObject],
                     description: String = "",
                     count: Int = 1): Unit = {
    val i = obj match {
      case Some(value) => value
      case None => top
    }
    mTopAllDropped = false
    mRaise(i, i, description, count)
  }

  def mDrop(obj: UVMObject,
            srcObj: UVMObject,
            description: String,
            count: Int = 1): Unit = {
    require(count > 0)

    if (obj == srcObj)
      mSourceCount.update(obj, mSourceCount(obj) - count)
    mTotalCount.update(obj, mTotalCount(obj) - count)

    // TODO: dropped
    if (mEvents.contains(obj))
      mEvents(obj).dropped.trigger()

    if (mTotalCount.contains(obj) && mTotalCount(obj) == 0) {
      if (mEvents.contains(obj))
        mEvents(obj).allDropped.trigger()
      mTopAllDropped = true
      if (obj == srcObj)
        mSourceCount -= obj
      mTotalCount -= obj
    }

    mProgagate(obj, srcObj, description, count, raise = false)
  }

  def dropObjection(obj: Option[UVMObject],
                    description: String = "",
                    count: Int = 1): Unit = {
    val i = obj match {
      case Some(value) => value
      case None => top
    }
    mTopAllDropped = false
    mDrop(i, i, description, count)
  }

  def waitFor(event: uvmObjectionEvent, obj: Option[UVMObject] = None): Unit = {
    val i = obj match {
      case Some(value) => value
      case None => top
    }

    val e = mEvents.getOrElseUpdate(i, UVMObjectionEvents(0,
      createEvent("raised"),
      createEvent("dropped"),
      createEvent("all_dropped")))

    e.waiters += 1
    event match {
      case ENUM_OBJECTION_EVENT.UVM_RAISED => ~>(e.raised)
      case ENUM_OBJECTION_EVENT.UVM_DROPPED => ~>(e.dropped)
      case ENUM_OBJECTION_EVENT.UVM_ALL_DROPPED => ~>(e.allDropped)
    }
    e.waiters -= 1
    if (e.waiters == 0)
      mEvents -= i
  }

  def getObjectionTotal(obj: Option[UVMObject] = None): Int = {
    val i = if (obj.nonEmpty) obj.get else top
    mTotalCount.getOrElse(i, 0)
  }

  setReportVerbosityLevel(top.getReportVerbosityLevel())
  mObjections += this
}

object UVMObjection {
  val mObjections = collection.mutable.Queue.empty[UVMObject]

}