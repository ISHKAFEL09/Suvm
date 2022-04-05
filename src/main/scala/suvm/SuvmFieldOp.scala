package suvm

import SuvmObjectGlobals._
import SuvmImplicits._

import scala.collection.mutable.ArrayBuffer

/**
 * SuvmFieldOp is the SUVM class for describing all operations supported by the doExecuteOp function
 * @param name
 */
class SuvmFieldOp(val name: String = "") extends SuvmObject {
  private var mIsSet: Boolean = false
  private var mUserHook: Boolean = true

  def set(opType: SuvmOpcodeEnum.Value, policy: Option[SuvmPolicy] = None, rhs: Option[SuvmObject] = None): Unit = {
    val matchingOps = ArrayBuffer.empty[String]

    def testOps(i: SuvmOpcodeEnum.Value): Unit =
      if (SuvmOpcodeEnum.hasOp(opType, i)) matchingOps += i.toString

    import SuvmOpcodeEnum._
    testOps(UVM_COPY)
    testOps(UVM_COMPARE)
    testOps(UVM_PRINT)
    testOps(UVM_RECORD)
    testOps(UVM_PACK)
    testOps(UVM_UNPACK)
    testOps(UVM_SET)

    println(matchingOps)
  }

  def flush(): Unit =
    ???

  def mRecycle(): Unit = {
    flush()
    SuvmFieldOp.mRecycledOp.enqueue(this)
  }
}

object SuvmFieldOp {
  private val mRecycledOp = scala.collection.mutable.Queue.empty[SuvmFieldOp]

  def getAvailableOp: SuvmFieldOp = {
    if (mRecycledOp.nonEmpty) mRecycledOp.dequeue()
    else new SuvmFieldOp("FieldOp")
  }
}

object SuvmFieldOpTest extends App {
  val fop = new SuvmFieldOp("fop")
  fop.set(SuvmOpcodeEnum.UVM_PRINT | SuvmOpcodeEnum.UVM_COMPARE)
}