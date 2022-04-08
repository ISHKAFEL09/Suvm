package suvm

import SuvmObjectGlobals._

import scala.collection.mutable

abstract class SuvmPortComponentBase(name: String,
                                     parent: Option[SuvmComponent]) extends SuvmComponent(name, parent)

class SuvmPortComponent(name: String,
                        parent: Option[SuvmComponent]) extends SuvmPortComponentBase(name, parent)

/**
 * IF is type of the interface implemented by derived port, export, implementation, or socket
 */
trait IF extends SuvmVoid
/**
 * Transaction-level communication between components is handled via its ports, exports, imps, and sockets,
 * all of which derive from this class.
 */
abstract class SuvmPortBase[T <: IF](name: String,
                                     parent: Option[SuvmComponent],
                                     portType: SuvmPortType.Value,
                                     minSize: Int = 0,
                                     maxSize: Int = 1) extends SuvmPortComponent(name, parent) {
  this: T =>

  /**
   * Returns the type name to this port.
   * Derived port classes can implement this method to return the concrete type.
   * Otherwise, only a generic "uvm_port", "uvm_export", or "uvm_implementation" is returned.
   */
  override def getTypeName: String = mPortType match {
    case SuvmPortType.UVM_PORT => "Port"
    case SuvmPortType.UVM_EXPORT => "Export"
    case SuvmPortType.UVM_IMPLEMENTATION => "Implementation"
  }

  /**
   * Returns the minimum number of implementation ports to be connected to this port
   * prior to resolve_bindings being called
   */
  def getMinSize: Int = mMinSize

  /**
   * Returns the maximum number of implementation ports to be connected to this port prior to
   * resolve_bindings being called
   */
  def getMaxSize: Int = mMaxSize

  /**
   * Returns 1 if this port has no maximum on the number of implementation ports this port can connect.
   */
  def isUnbounded: Boolean = mMaxSize == -1

  /**
   * For a port or export type, this function intends to fill a list with all of the
   * ports, exports and implementations that this port is connected to.
   */
  def getConnectedTo: mutable.Map[String, SuvmPortBase[T]] = mProvidedBy

  /**For an implementation or export type, this function intends to fill a list with all of
   * the ports, exports, and implementations to which this port has provided its implementation.
   */
  def getProvidedTo: mutable.Map[String, SuvmPortBase[T]] = mProvidedTo

  def isPort: Boolean = mPortType == SuvmPortType.UVM_PORT

  def isExport: Boolean = mPortType == SuvmPortType.UVM_EXPORT

  def isImp: Boolean = mPortType == SuvmPortType.UVM_IMPLEMENTATION

  /**
   * @return the number of implementation ports connected to this port.
   */
  def size: Int = mImpList.size

  /**
   * Specifies the default implementation port to use when calling an interface method.
   */
//  def setDefaultIndex(index: Int): Unit = mDefIndex = index

  /**
   * Connects this port to the given provider port.
   */
  def connect(provider: SuvmPortBase[T]): Unit = {
    SuvmEndOfElaborationPhase.getState match {
      case SuvmPhaseState.UVM_PHASE_EXECUTING | SuvmPhaseState.UVM_PHASE_DONE =>
        suvmReportWarning("Late Connection", s"Attempt to connect $getFullName(of type $getTypeName)" +
          s" at or after end_of_elaboration phase.  Ignoring.")
      case _ =>
    }
    if (provider == this) {
      suvmReportError(sConnectionErrorId, "Cannot connect a port instance to itself", SuvmVerbosity.UVM_NONE)
    } else if ((provider.mIfMask & mIfMask) != mIfMask) {
      suvmReportError(sConnectionErrorId, s"${provider.getFullName} (of type ${provider.getTypeName}) does " +
        s"not provide the complete interface required of this port (type $getTypeName)", SuvmVerbosity.UVM_NONE)
    } else if (isImp) {
      suvmReportError(sConnectionErrorId, s"Cannot call an imp port's connect method. An imp is connected " +
        s"only to the component passed in its constructor. (You attempted to bind this " +
        s"imp to ${provider.getFullName})", SuvmVerbosity.UVM_NONE)
    } else if (isExport && provider.isPort) {
      suvmReportError(sConnectionErrorId, s"Cannot connect exports to ports Try calling port.connect(export)" +
        s" instead. (You attempted to bind this export to ${provider.getFullName})", SuvmVerbosity.UVM_NONE)
    } else {
      mCheckRelationship(provider)
      mProvidedBy.update(provider.getFullName, provider)
      provider.mProvidedTo.update(getFullName, this)
    }
  }

  /**
   * automatically called just before entering the end_of_elaboration phase.
   * It recurses through each port’s fanout to determine all the imp destinations.
   * It then checks against the required minimum and maximum connections.
   */
  def resolveBindings(): Unit = {
    if (!mResolved) {
      if (isImp) mImpList.update(getFullName, this)
      else for ((_, v) <- mProvidedBy) {
        v.resolveBindings()
        mAddList(v)
      }
    }
    mResolved = true
    if (size < minSize) suvmReportError(sConnectionErrorId, s"connection count of $size does not meet " +
      s"required minimum of $minSize", SuvmVerbosity.UVM_NONE)
    if (maxSize != -1 && size > maxSize) suvmReportError(sConnectionErrorId, s"connection count of $size " +
      s"exceeds maximum of $maxSize", SuvmVerbosity.UVM_NONE)
  }

  /**
   * @return an implementation (imp) port at the given index from the array of imps to which this port is connected.
   */
  def getIf(i: Int): Option[SuvmPortBase[T]] = {
    if (size == 0) {
      suvmReportWarning("getIf", "Port size is zero", SuvmVerbosity.UVM_NONE)
      None
    } else if (i < 0 || i >= size) {
      suvmReportWarning("getIf", s"Index $i out of range [0,$size)", SuvmVerbosity.UVM_NONE)
      None
    } else Some(mImpList(mImpList.keys.toIndexedSeq(i)))
  }

  /**
   * Checks that the connection is between ports that are hierarchically
   * adjacent (up or down one level max, or are siblings),
   * and check for legal direction, requirer.connect(provider).
   */
  private def mCheckRelationship(provider: SuvmPortBase[T]): Boolean = {
    if (getTypeName == "SuvmAnalysisPort" || getParent.isEmpty || provider.getParent.isEmpty) true
    else if (isPort && provider.isPort && getParent.get.getParent != provider.getParent) {
      suvmReportWarning(sConnectionWarningId, s"${provider.getFullName} (of type ${provider.getTypeName})" +
        s" is not up one level of hierarchy from this port. ")
      false
    } else if (isPort && !provider.isPort && getParent.get.getParent != provider.getParent.get.getParent) {
      suvmReportWarning(sConnectionWarningId, s"${provider.getFullName} (of type ${provider.getTypeName})" +
        s" is not at the same level of hierarchy as this port")
      false
    } else if (isExport && !provider.isPort && getParent !=  provider.getParent.get.getParent) {
      suvmReportWarning(sConnectionWarningId, s"${provider.getFullName} (of type ${provider.getTypeName})" +
        s" is not down one level of hierarchy from this export")
      false
    } else true
  }

  private def mAddList(provider: SuvmPortBase[T]): Unit = {
    0 until provider.size foreach { i =>
      val imp = provider.getIf(i)
      if (imp.nonEmpty)
        mImpList.getOrElseUpdate(imp.get.getFullName, imp.get)
    }
  }

  private val sConnectionWarningId = "Connection Warning"
  private val sConnectionErrorId = "Connection error"
  protected val mIfMask: Int = 0
  private var mResolved: Boolean = false
  private val mProvidedBy = collection.mutable.Map.empty[String, SuvmPortBase[T]]
  private val mProvidedTo = collection.mutable.Map.empty[String, SuvmPortBase[T]]
  private val mImpList = collection.mutable.Map.empty[String, SuvmPortBase[T]]
  private val mPortType: SuvmPortType.Value = portType
  private val mMinSize: Int = minSize
  private val mMaxSize: Int = maxSize
  val (x, _) = SuvmConfigDbInt.get(this, "", "checkConnectionRelationships", 0)
  if (!x) setReportIdAction(sConnectionWarningId, SuvmActionType.UVM_NO_ACTION)
}

object SuvmPortBaseTest extends App {
  trait Port0 extends IF
  trait Port1 extends IF

  val comp0 = new SuvmComponent("comp0") {}
  val comp1 = new SuvmComponent("comp1", Some(comp0)) {}
  val comp2 = new SuvmComponent("comp2") {}
  val comp3 = new SuvmComponent("comp3", Some(comp2)) {}

  class MyPort0(name: String, parent: Option[SuvmComponent], portType: SuvmPortType.Value)
    extends SuvmPortBase[Port0](name, parent, portType) with Port0

  class MyPort1(name: String, parent: Option[SuvmComponent], portType: SuvmPortType.Value)
    extends SuvmPortBase[Port0](name, parent, portType) with Port0

  val a = new MyPort0("p0", Some(comp0), SuvmPortType.UVM_PORT)
  val b = new MyPort0("p1", Some(comp1), SuvmPortType.UVM_PORT)
  val c = new MyPort1("p2", Some(comp0), SuvmPortType.UVM_PORT)
  val d = new MyPort0("p3", Some(comp2), SuvmPortType.UVM_EXPORT)
  val e = new MyPort0("p4", Some(comp3), SuvmPortType.UVM_IMPLEMENTATION)

//  a.connect(c)
//  SuvmEndOfElaborationPhase.setState(SuvmPhaseState.UVM_PHASE_DONE)
  b.connect(a)
  a.connect(d)
  d.connect(e)
  println(b.size)
  b.resolveBindings()
  println(b.size)
  println(a.size)
}