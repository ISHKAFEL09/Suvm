package suvm

/**
 * base class for user-defined callback classes.
 * Typically, the component developer defines an application-specific callback class
 * that extends from this class. In it, one or more virtual methods are defined (callback interfaces)
 * that represent the hooks available for user override.
 */
class SuvmCallback(val name: String = "SuvmCallback") extends SuvmObject {
  /**
   * Enables/disables callbacks: on==0 disables, on==1 enables. Any value for on other
   * than 0 or 1 has no effect on the enable state of the callback.
   */
  def callbackMode(on: Int = -1): Boolean = {
    val x = mEnabled
    mEnabled = if (on == 0) false else if(on == 1) true else x
    x
  }

  def isEnabled: Boolean = callbackMode()

  private var mEnabled: Boolean = true
}

object SuvmCallback extends SuvmObjectUtils[SuvmCallback] {
  override def create: String => SuvmCallback = new SuvmCallback(_)
}

object SuvmTypeIdBase {
  val typeIdMap = new collection.mutable.HashMap[SuvmObject, SuvmTypedCallbacks]
  val typeMap = new collection.mutable.HashMap[SuvmTypedCallbacks, SuvmObject]
}

object SuvmCallbacksBase extends SuvmObject {
  override val name: String = _
}

trait SuvmTypedCallbacks extends SuvmObject {
  type T <: SuvmObject
  var mTracing: Boolean
  val mbInst: SuvmTypedCallbacks = this
  val mPool: SuvmPool[SuvmObject, SuvmQueue[SuvmCallback]] =
    new SuvmPool[SuvmObject, SuvmQueue[SuvmCallback]] {
      override def defaultValue(key: SuvmObject): SuvmQueue[SuvmCallback] = new SuvmQueue[SuvmCallback]
    }
  val mThisType = new SuvmQueue[SuvmTypedCallbacks]
  var mSuperType: SuvmTypeId = _
  val mDerivedTypes = new SuvmQueue[SuvmTypeId]

  def mIsRegistered(obj: Option[SuvmObject], cb: SuvmCallback): Boolean

  def checkRegistration(obj: Option[SuvmObject], cb: SuvmCallback): Boolean = {
    mIsRegistered(obj, cb) | { mThisType.exists { i => mbInst != i && i.mIsRegistered(obj, cb) } } | {
      obj.isEmpty && mDerivedTypes.exists { i => {
        SuvmTypeIdBase.typeIdMap.contains(i) && SuvmTypeIdBase.typeIdMap(i).checkRegistration(None, cb)
      }}
    }
  }
  val mTwCbQ = new SuvmQueue[SuvmCallback]
  var mTypeName: String = _
  val mtInst
}

/**
 * The ~uvm_callbacks~ class provides a base class for implementing callbacks,
 * which are typically used to modify or augment component behavior without
 * changing the component class. To work effectively, the developer of the
 * component class defines a set of "hook" methods that enable users to
 * customize certain behaviors of the component in a manner that is controlled
 * by the component developer. The integrity of the component's overall behavior
 * is intact, while still allowing certain customizable actions by the user.
 */
class SuvmCallbacks[T <: SuvmObject, CB <: SuvmCallback]