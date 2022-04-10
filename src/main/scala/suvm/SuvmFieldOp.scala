package suvm

import SuvmObjectGlobals._
import SuvmImplicits._

import scala.collection.mutable.ArrayBuffer

/**
 * SuvmFieldOp is the SUVM class for describing all operations supported by the doExecuteOp function
 */
class SuvmFieldOp(val name: String = "") extends SuvmObject {
  /**
   * set the operation op_type, policy and rhs values
   */
  def set(opType: SuvmOpcodeEnum.Value, policy: Option[SuvmPolicy] = None, rhs: Option[SuvmObject] = None): Unit = {
    val matchingOps = ArrayBuffer.empty[String]

    def testOps(i: SuvmOpcodeEnum.Value): Unit = if (opType.hasOp(i)) matchingOps += i.toString

    import SuvmOpcodeEnum._
    testOps(UVM_COPY)
    testOps(UVM_COMPARE)
    testOps(UVM_PRINT)
    testOps(UVM_RECORD)
    testOps(UVM_PACK)
    testOps(UVM_UNPACK)
    testOps(UVM_SET)

    if (matchingOps.size > 1)
      suvmError("UVM/FIELD_OP/SET_BAD_OP_TYPE",
        "set() was passed op_type matching multiple operations: " + matchingOps.mkString("(", ", ", ")"))

    if (!mIsSet) {
      mOpType = opType
      mPolicy = policy
      mObject = rhs
      mIsSet = true
    } else {
      suvmError("UVM/FIELD_OP/SET","Attempting to set values in policy without flushing")
    }
  }

  def getOpName: String = {
    import SuvmOpcodeEnum._
    mOpType match {
      case UVM_COPY => "copy"
      case UVM_COMPARE => "compare"
      case UVM_PRINT => "print"
      case UVM_RECORD => "record"
      case UVM_PACK => "pack"
      case UVM_UNPACK => "unpack"
      case UVM_SET => "set"
      case _ => ""
    }
  }

  def getOpType: Option[SuvmOpcodeEnum.Value] =
    if (mIsSet) Some(mOpType) else {
      suvmError("UVM/FIELD_OP/GET_OP_TYPE","Calling get_op_type() before calling set() is not allowed")
      None
    }

  def getPolicy: Option[SuvmPolicy] =
    if (mIsSet) mPolicy else {
      suvmError("UVM/FIELD_OP/GET_POLICY","Calling getPolicy() before calling set() is not allowed")
      None
    }

  def getRhs: Option[SuvmObject] =
    if (mIsSet) mObject else {
      suvmError("UVM/FIELD_OP/GET_RHS","Calling getRhs() before calling set() is not allowed")
      None
    }

  def userHookEnabled: Boolean = {
    if (!mIsSet)
      suvmError("UVM/FIELD_OP/GET_USER_HOOK","Calling userHookEnabled() before calling set() is not allowed")
    mUserHook
  }

  def disableUserHook(): Unit = mUserHook = false

  def flush(): Unit = {
    mPolicy = None
    mObject = None
    mUserHook = true
    mIsSet = false
  }

  def mRecycle(): Unit = {
    flush()
    SuvmFieldOp.mRecycledOp.enqueue(this)
  }

  private var mIsSet: Boolean = false
  private var mUserHook: Boolean = true
  private var mOpType: SuvmOpcodeEnum.Value = SuvmOpcodeEnum.UVM_DEFAULT
  private var mPolicy: Option[SuvmPolicy] = None
  private var mObject: Option[SuvmObject] = None
}

object SuvmFieldOp extends SuvmObjectUtils[SuvmFieldOp] {
  override def create = new SuvmFieldOp(_)

  private val mRecycledOp = scala.collection.mutable.Queue.empty[SuvmFieldOp]

  def mGetAvailableOp: SuvmFieldOp = {
    if (mRecycledOp.nonEmpty) mRecycledOp.dequeue()
    else typeId.create("FieldOp")
  }
}

object SuvmFieldOpTest extends App {
  val fop = SuvmFieldOp.typeId.create("fop")
  fop.set(SuvmOpcodeEnum.UVM_PRINT | SuvmOpcodeEnum.UVM_COMPARE)
  println(fop)
}