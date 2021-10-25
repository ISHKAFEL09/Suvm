package suvm

import scala.util.Random

sealed abstract class SuvmVoid

abstract class SuvmObject(val name: String = "") extends SuvmVoid {
  private var mLeafName = name
  private val mInstId = { SuvmObject.mInstCount += 1; SuvmObject.mInstCount }
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

  def createObj(name: String): Option[SuvmObject]
  def cloneObj: Option[SuvmObject] = createObj(getName) flatMap { s => s.copyObj(Some(this)) }

  final def printObj(printer: Option[SuvmPrinter] = None): Unit = {
    val mPrinter = if (printer.isEmpty) SuvmPrinter.getDefault else printer.get
    fWrite(mPrinter.getFile, sPrintObj(Some(mPrinter)))
  }
  final def sPrintObj(printer: Option[SuvmPrinter] = None): String = {
    val mPrinter = if (printer.isEmpty) SuvmPrinter.getDefault else printer.get
    val name: String = if (!mPrinter.getActiveObjectDepth) {
      mPrinter.flush()
      if (mPrinter.getRootEnabled) getFullName else getName
    } else getName
    mPrinter.printObject(name, this)
    mPrinter.emit
  }
  def doPrint(printer: SuvmPrinter): Unit = {}
  def convert2String: String = ""

  final def record(recorder: Option[SuvmRecorder] = None): Unit = {}
  def doRecord(recorder: SuvmRecorder): Unit = {}

  final def copyObj(rhs: Option[SuvmObject], copier: Option[SuvmCopier] = None): Option[SuvmObject] = rhs flatMap { i =>
      val mCopier = if (copier.isEmpty) SuvmCoreService.getDefaultCopier else copier.get
      mCopier.copyObject(this, i)
  }
  def doCopy(rhs: SuvmObject): Unit = {}

  final def compareObj(rhs: SuvmObject, comparer: Option[SuvmComparer] = None): Boolean = {
    val mComparer = if (comparer.isEmpty) SuvmComparer.getDefault else comparer.get
    if (!mComparer.getActiveObjectDepth) mComparer.flush()
    mComparer.compareObject(getName, this, rhs)
  }
  def doCompare(rhs: SuvmObject, comparer: SuvmComparer): Boolean = true

  private def pack[T](packer: Option[SuvmPacker] = None): (Int, Seq[T]) = {
    val mPacker = mPack(packer)
    (mPacker.getPackedSize, mPacker.getPacked[T])
  }
  final def packBits(packer: Option[SuvmPacker] = None): (Int, Seq[Boolean]) = pack[Boolean](packer)
  final def packBytes(packer: Option[SuvmPacker] = None): (Int, Seq[Byte]) = pack[Byte](packer)
  final def packInts(packer: Option[SuvmPacker] = None): (Int, Seq[Int]) = pack[Int](packer)
  final def packLongs(packer: Option[SuvmPacker] = None): (Int, Seq[Long]) = pack[Long](packer)
  def doPack(packer: SuvmPacker): Unit = {}

  private def unPack[T](s: Seq[T], packer: Option[SuvmPacker] = None): (Int, Seq[T]) = {
    val mPacker = mUnpackPre(packer)
    val stream = mPacker.setPacked[T](s)
    (mUnpackPost(mPacker), stream)
  }
  final def unPackBits(s: Seq[Boolean], packer: Option[SuvmPacker] = None): (Int, Seq[Boolean]) =
    unPack[Boolean](s, packer)
  final def unPackBytes(s: Seq[Byte], packer: Option[SuvmPacker] = None): (Int, Seq[Byte]) =
    unPack[Byte](s, packer)
  final def unPackInts(s: Seq[Int], packer: Option[SuvmPacker] = None): (Int, Seq[Int]) =
    unPack[Int](s, packer)
  final def unPackLongs(s: Seq[Long], packer: Option[SuvmPacker] = None): (Int, Seq[Long]) =
    unPack[Long](s, packer)
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
    val mPacker = if (packer.isEmpty) SuvmPacker.getDefault else packer.get
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
  private var mInstCount: Int = 0

  def getInstCount: Int = mInstCount
  def getType: Option[SuvmObjectWrapper] = {
    SuvmReportError("NOTYPID", "get_type not implemented in derived class." , SuvmVerbosity.UVM_NONE)
    None
  }
}

object SuvmObjectTest extends App {
}