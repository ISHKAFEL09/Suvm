package suvm

import scala.util.Random
import SuvmObjectGlobals._

sealed abstract class SuvmVoid

abstract class SuvmObject extends SuvmVoid {
  val name: String
  private var mLeafName = name
  private val mInstId =
    SuvmObject.setInstCount(this.getClass.getName, SuvmObject.getInstCount(this.getClass.getName) + 1)
  private lazy val rng = new Random()

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

  def getSuvmSeeding: Boolean = SuvmCoreService.getSuvmSeeding
  def setSuvmSeeding(enable: Boolean): Unit = SuvmCoreService.setSuvmSeeding(enable)

  def reseed(): Unit = if (getSuvmSeeding) rng.setSeed(createRandomSeed(getTypeName, getFullName))

  def setName(name: String): Unit = mLeafName = name
  def getName: String = mLeafName
  def getFullName: String = getName
  def getTypeName: String = "<unknown>"
  def getObjectType: Option[SuvmObjectWrapper] = if (getTypeName == "<unknown>") None else
    SuvmFactory.findWrapperByName(getTypeName)
  def getInstId: Int = mInstId

  def createObj(name: String): Option[SuvmObject] = None
  def cloneObj: Option[SuvmObject] = createObj(getName) flatMap { s => s.copyObj(Some(this)) }

  final def printObj(printer: Option[SuvmPrinter] = None): Unit = {
    val mPrinter = printer getOrElse SuvmPrinter.getDefault
    fWrite(mPrinter.getFile, sPrintObj(Some(mPrinter)))
  }
  final def sPrintObj(printer: Option[SuvmPrinter] = None): String = {
    val mPrinter = printer getOrElse SuvmPrinter.getDefault
    val name: String = if (!mPrinter.getActiveObjectDepth) {
      mPrinter.flush()
      if (mPrinter.getRootEnabled) getFullName else getName
    } else getName
    mPrinter.printObject(name, this)
    mPrinter.emit
  }
  def doPrint(printer: SuvmPrinter): Unit = {}
  def convert2String: String = ""

  final def recordObj(recorder: Option[SuvmRecorder] = None): Unit = {}
  def doRecord(recorder: SuvmRecorder): Unit = {}

  final def copyObj(rhs: Option[SuvmObject], copier: Option[SuvmCopier] = None): Option[SuvmObject] = rhs flatMap { i =>
      val mCopier = copier getOrElse SuvmCoreService.getDefaultCopier
      mCopier.copyObject(this, i)
  }
  def doCopy(rhs: SuvmObject): Unit = {}

  final def compareObj(rhs: SuvmObject, comparer: Option[SuvmComparer] = None): Boolean = {
    val mComparer = comparer getOrElse SuvmComparer.getDefault
    if (!mComparer.getActiveObjectDepth) mComparer.flush()
    mComparer.compareObject(getName, this, rhs)
  }
  def doCompare(rhs: SuvmObject, comparer: SuvmComparer): Boolean = true

  private def packObj[T](packer: Option[SuvmPacker] = None, f: SuvmPacker => Seq[T]): (Int, Seq[T]) = {
    val mPacker = mPack(packer)
    (mPacker.getPackedSize, f(mPacker))
  }
  final def packBits(packer: Option[SuvmPacker] = None): (Int, Seq[Boolean]) =
    packObj(packer, p => p.getPackedBool)
  final def packBytes(packer: Option[SuvmPacker] = None): (Int, Seq[Byte]) =
    packObj(packer, p => p.getPackedByte)
  final def packInts(packer: Option[SuvmPacker] = None): (Int, Seq[Int]) =
    packObj(packer, p => p.getPackedInt)
  final def packLongs(packer: Option[SuvmPacker] = None): (Int, Seq[Long]) =
    packObj(packer, p => p.getPackedLong)
  def doPack(packer: SuvmPacker): Unit = {}

  private def unPack[T](s: Seq[T], packer: Option[SuvmPacker] = None): (Int, Seq[T]) = {
    val mPacker = mUnpackPre(packer)
    val stream = mPacker.setPacked[T](s)
    (mUnpackPost(mPacker), stream)
  }
  final def unPackBits(s: Seq[Boolean], packer: Option[SuvmPacker] = None): (Int, Seq[Boolean]) =
    unPack(s, packer)
  final def unPackBytes(s: Seq[Byte], packer: Option[SuvmPacker] = None): (Int, Seq[Byte]) =
    unPack(s, packer)
  final def unPackInts(s: Seq[Int], packer: Option[SuvmPacker] = None): (Int, Seq[Int]) =
    unPack(s, packer)
  final def unPackLongs(s: Seq[Long], packer: Option[SuvmPacker] = None): (Int, Seq[Long]) =
    unPack(s, packer)
  def doUnpack(packer: SuvmPacker): Unit = {}

  def doExecuteOp(op: SuvmFieldOp): Unit = {}

  def setLocal(rsrc: Option[SuvmResourceBase]): Unit = {
    if (rsrc.nonEmpty) {
      val op = SuvmFieldOp.getAvailableOp
      op.set(SuvmOpcodeEnum.UVM_SET, None, rsrc)
      doExecuteOp(op)
      op.mRecycle()
    }
  }
  private def mUnsupportedSetLocal(rsrc: SuvmResourceBase): Unit = {}

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
  private def _mSuvmFieldAutomation(tmpData: SuvmObject, str: String): Unit = {}
  private def SuvmGetReportObject: Option[SuvmReportObject] = None
}

object SuvmObject {
  private val _mInstCountMap = collection.mutable.HashMap.empty[String, Int]

  def getInstCount(s: String): Int = _mInstCountMap.getOrElseUpdate(s, 0)
  def setInstCount(s: String, i: Int): Int = {_mInstCountMap.update(s, i); i}

  // TODO factory type
  def getType: Option[SuvmObjectWrapper] = {
    suvmReportError("NOTYPID", "get_type not implemented in derived class." , SuvmVerbosity.UVM_NONE)
    None
  }
}

object SuvmObjectTest extends App {
}