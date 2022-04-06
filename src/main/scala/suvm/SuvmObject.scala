package suvm

import scala.util.Random
import SuvmObjectGlobals._

/**
 * base class for all Suvm classes
 */
abstract class SuvmVoid

/**
 * The SuvmObject class is the abstract base class for all SUVM data and hierarchical classes.
 * Its primary role is to define a set of methods for such common operations as create, copy,
 * compare, print, and record.
 * Classes deriving from SuvmObject need to implement the abstract methods such as create and getTypeName.
 */
abstract class SuvmObject extends SuvmVoid {
  val name: String

  /**
   * seeding
   */
  private lazy val rng = new Random()

  def getSuvmSeeding: Boolean = SuvmCoreService.getSuvmSeeding

  def setSuvmSeeding(enable: Boolean): Unit = SuvmCoreService.setSuvmSeeding(enable)

  def reseed(): Unit = if (getSuvmSeeding) rng.setSeed(createRandomSeed(getTypeName, getFullName))

  def createRandomSeed(typeId: String, instId: String): Int = {
    val mInstId = if (instId.isEmpty) "__global__" else instId
    val seedMap = SuvmRandomSeedTableLookup.getOrElseUpdate(mInstId, new SuvmSeedMap)
    val mTypeId = f"$typeId::${this.hashCode().toString}"
    seedMap.seedTable.getOrElseUpdate(mTypeId, f"$mTypeId::$mInstId$SuvmGlobalRandomSeed".hashCode)
    seedMap.count.getOrElseUpdate(mTypeId, 0)
    seedMap.seedTable(mTypeId) = seedMap.seedTable(mTypeId) + seedMap.count(mTypeId)
    seedMap.count(mTypeId) = seedMap.count(mTypeId) + 1
    seedMap.seedTable(mTypeId)
  }

  /**
   * identification
   */
  private var mLeafName = name
  private val mInstId = {
    SuvmObject.setInstCount(this.getClass.getName, SuvmObject.getInstCount(this.getClass.getName) + 1)
  }

  def setName(name: String): Unit = mLeafName = name

  def getName: String = mLeafName

  def getFullName: String = getName

  def getTypeName: String = "<unknown>"

  def getObjectType: Option[SuvmObjectWrapper] = if (getTypeName == "<unknown>") None else
    SuvmFactory.findWrapperByName(getTypeName)

  def getInstId: Int = mInstId

  def getType: Option[SuvmObjectWrapper] = {
    suvmReportError("NOTYPID", "get_type not implemented in derived class." , SuvmVerbosity.UVM_NONE)
    None
  }

  override def toString: String = getClass.getSimpleName

  /**
   * creation
   */
  def createObj(name: String): Option[SuvmObject] = None

  def cloneObj: Option[SuvmObject] = createObj(getName) flatMap { s => s.copyObj(Some(this)) }

  /**
   * printing
   */
  def printObj(printer: Option[SuvmPrinter] = None): Unit = {
    val mPrinter = printer getOrElse SuvmPrinter.getDefault
    fWrite(mPrinter.getFile, sPrintObj(Some(mPrinter)))
  }

  def sPrintObj(printer: Option[SuvmPrinter] = None): String = {
    val mPrinter = printer getOrElse SuvmPrinter.getDefault
    val name: String = if (!mPrinter.getActiveObjectDepth) {
      mPrinter.flush()
      if (mPrinter.getRootEnabled) getFullName else getName
    } else getName
    mPrinter.printObject(name, this)
    mPrinter.emit
  }

  def doPrint(printer: Some[SuvmPrinter]): Unit = {}

  def convert2String: String = ""

  /**
   * recording
   */
  def recordObj(recorder: Option[SuvmRecorder] = None): Unit = {}

  def doRecord(recorder: Some[SuvmRecorder]): Unit = {}

  /**
   * copying
   */
  def copyObj(rhs: Option[SuvmObject], copier: Option[SuvmCopier] = None): Option[SuvmObject] = rhs flatMap { i =>
    val mCopier = copier getOrElse SuvmCoreService.getDefaultCopier
    mCopier.copyObject(this, i)
  }

  def doCopy(rhs: Some[SuvmObject]): Unit = {}

  /**
   * comparing
   */
  def compareObj(rhs: SuvmObject, comparer: Option[SuvmComparer] = None): Boolean = {
    val mComparer = comparer getOrElse SuvmComparer.getDefault
    if (!mComparer.getActiveObjectDepth) mComparer.flush()
    mComparer.compareObject(getName, this, rhs)
  }

  def doCompare(rhs: SuvmObject, comparer: SuvmComparer): Boolean = true

  /**
   * packing
   */
  private def mPack(packer: Option[SuvmPacker]): SuvmPacker = {
    val mPacker = packer getOrElse SuvmPacker.getDefault
    if (mPacker.getActiveObjectDepth) mPacker.flush()
    mPacker.packObject(this)
  }

  private def mUnpackPre(packer: Option[SuvmPacker]): SuvmPacker = {
    val mPacker = if (packer.isEmpty) SuvmPacker.getDefault else packer.get
    if (mPacker.getActiveObjectDepth) mPacker.flush()
    mPacker
  }

  private def mUnpackPost(packer: SuvmPacker): Int = {
    val sizeBeforeUnpack = packer.getPackedSize
    packer.unPackObject(this)
    sizeBeforeUnpack - packer.getPackedSize
  }

  private def packObj[T](packer: Option[SuvmPacker] = None, f: SuvmPacker => Seq[T]): (Int, Seq[T]) = {
    val mPacker = mPack(packer)
    (mPacker.getPackedSize, f(mPacker))
  }

  def packBits(packer: Option[SuvmPacker] = None): (Int, Seq[Boolean]) =
    packObj(packer, p => p.getPackedBool)

  def packBytes(packer: Option[SuvmPacker] = None): (Int, Seq[Byte]) =
    packObj(packer, p => p.getPackedByte)

  def packInts(packer: Option[SuvmPacker] = None): (Int, Seq[Int]) =
    packObj(packer, p => p.getPackedInt)

  def packLongs(packer: Option[SuvmPacker] = None): (Int, Seq[Long]) =
    packObj(packer, p => p.getPackedLong)

  def doPack(packer: SuvmPacker): Unit = {}

  private def unPack[T](s: Seq[T], packer: Option[SuvmPacker] = None): (Int, Seq[T]) = {
    val mPacker = mUnpackPre(packer)
    val stream = mPacker.setPacked[T](s)
    (mUnpackPost(mPacker), stream)
  }

  def unPackBits(s: Seq[Boolean], packer: Option[SuvmPacker] = None): (Int, Seq[Boolean]) =
    unPack(s, packer)

  def unPackBytes(s: Seq[Byte], packer: Option[SuvmPacker] = None): (Int, Seq[Byte]) =
    unPack(s, packer)

  def unPackInts(s: Seq[Int], packer: Option[SuvmPacker] = None): (Int, Seq[Int]) =
    unPack(s, packer)

  def unPackLongs(s: Seq[Long], packer: Option[SuvmPacker] = None): (Int, Seq[Long]) =
    unPack(s, packer)

  def doUnpack(packer: SuvmPacker): Unit = {}

  /**
   * configuration
   */
  def setLocal(rsrc: Option[SuvmResourceBase]): Unit = {
    if (rsrc.nonEmpty) {
      val op = SuvmFieldOp.mGetAvailableOp
      op.set(SuvmOpcodeEnum.UVM_SET, None, rsrc)
      doExecuteOp(op)
      op.mRecycle()
    }
  }

  /**
   * field operations
   */
  def doExecuteOp(op: SuvmFieldOp): Unit = {}
}

object SuvmObject {
  private val _mInstCountMap = collection.mutable.HashMap.empty[String, Int]

  def listObj(): Unit = println(_mInstCountMap)
  def getInstCount(s: String): Int = _mInstCountMap.getOrElseUpdate(s, 0)
  def setInstCount(s: String, i: Int): Int = {_mInstCountMap.update(s, i); i}
}

object SuvmObjectTest extends App {
  SuvmObject.listObj()

  class Scoreboard extends SuvmObject {
    val name = "Scoreboard"
  }

  val obj = new Scoreboard

  SuvmObject.listObj()

  val obj1 = new Scoreboard

  SuvmObject.listObj()
}