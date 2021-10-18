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

  // TODO
//  final def printObj(printer: Option[SuvmPrinter] = None): Unit = {}
//  final def sPrintObj(printer: Option[SuvmPrinter] = None): Unit = {}
//  def doPrint(printer: SuvmPrinter): Unit
  def convert2String: String = ""

  final def record(recorder: Option[SuvmRecorder] = None): Unit = {}
  def doRecord(recorder: SuvmRecorder): Unit

  final def copyObj(rhs: Option[SuvmObject], copier: Option[SuvmCopier] = None): Option[SuvmObject] = rhs flatMap { i =>
      val mCopier = if (copier.isEmpty) SuvmCoreService.getDefaultCopier else copier.get
      mCopier.copyObject(this, i)
  }
  def doCopy(rhs: SuvmObject): Unit

  final def compareObj(rhs: SuvmObject, comparer: Option[SuvmComparer] = None): Boolean = { false }
  def doCompare(rhs: SuvmObject, comparer: SuvmComparer): Boolean

  final def packObj(bitstream: Seq[Boolean], packer: Option[SuvmPacker] = None): (Int, Seq[Boolean])
  final def packBytesObj(bitstream: Seq[Byte], packer: Option[SuvmPacker] = None): (Int, Seq[Byte])
  final def packIntsObj(bitstream: Seq[Int], packer: Option[SuvmPacker] = None): (Int, Seq[Int])
  final def packLongsObj(bitstream: Seq[Long], packer: Option[SuvmPacker] = None): (Int, Seq[Long])
  def doPack(packer: SuvmPacker): Unit

  final def unPackObj(bitstream: Seq[Boolean], packer: Option[SuvmPacker] = None): (Int, Seq[Boolean])
  final def unPackBytesObj(bitstream: Seq[Byte], packer: Option[SuvmPacker] = None): (Int, Seq[Byte])
  final def unPackIntsObj(bitstream: Seq[Int], packer: Option[SuvmPacker] = None): (Int, Seq[Int])
  final def unPackLongsObj(bitstream: Seq[Long], packer: Option[SuvmPacker] = None): (Int, Seq[Long])
  def doUnpack(packer: SuvmPacker): Unit

  def doExecuteOp(op: SuvmFieldOp): Unit

  // TODO
//  def setLocal(rsrc: SuvmResourceBase): Unit
//  private def mUnsupportedSetLocal(rsrc: SuvmResourceBase): Unit = {}

  private def mPack(packer: SuvmPacker): SuvmPacker
  private def mUnpackPre(packer: SuvmPacker): SuvmPacker
  private def mUnpackPost(packer: SuvmPacker): SuvmPacker
  private def _mSuvmFieldAutomation(tmpData: SuvmObject, str: String)
  private def SuvmGetReportObject: SuvmReportObject
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