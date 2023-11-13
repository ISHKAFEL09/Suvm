package rockets.tilelink

import rockets.core.consts.MemOps._
import rockets.params._
import spinal.core._
import rockets.params.config._
import rockets.utils._

abstract class TLBundle(implicit val p: Parameters)
    extends Bundle
    with HasLinkParams
    with HasCoreParams
    with HasTileParams

abstract class TLComponent(implicit val p: Parameters)
    extends Component
    with HasLinkParams
    with HasCoreParams
    with HasTileParams

/** base trait for all tl channels, A ~ E */
trait TLChannel extends TLBundle {

  /** current xact has data */
  def hasData: Bool

  /** current xact has multi beat data */
  def hasDataMultiBeat: Bool
}

/** base trait for channel from client to manager, A/C/E */
trait C2MChannel extends TLChannel

/** base trait for channel from manager to client, B/D */
trait M2CChannel extends TLChannel

/** address of a cache block */
trait HasTLBlockAddress extends TLBundle {
  val blockAddr = UInt(tlBlockAddrBits bits)

  def conflicts(that: HasTLBlockAddress): Bool =
    blockAddr === that.blockAddr

  def conflicts(addr: UInt): Bool = blockAddr === addr
}

/** sub-block address, beat id of multi-beat data */
trait HasTLBeatAddress extends TLBundle {
  val beatAddr = UInt(tlBeatAddrBits bits)
}

/** client side xact id, same as MSHR index */
trait HasTLClientXactId extends TLBundle {
  val xactId = UInt(tlClientXactIdBits bits)
}

/** manager side xact id? */
trait HasManagerXactId extends TLBundle {
  val xactId = UInt(tlManagerXactIdBits bits)
}

/** a single beat data */
trait HasTLData extends HasTLBeatAddress {
  val data = UInt(tlDataBits bits)

  /** current xact has data */
  def hasData: Bool

  /** current xact has multi beat data */
  def hasDataMultiBeat: Bool
}

/** id of client, used in managers */
trait HasTLClientId extends TLBundle {
  val clientId = UInt(tlClientIdBits bits)
}

/** tilelink channel definitions */

/** A channel, initiate xact to gain access to a block's data with certain permission
  * messages may be custom types for cached data or built-in types for uncached data
  * may contain data for put or PutAtomic built-in types
  * after sent, must wait for a manager to send a Grant
  */
class Acquire(implicit p: Parameters)
    extends C2MChannel
    with HasTLData
    with HasTLBlockAddress
    with HasTLClientXactId {

  /** whether is build-in non-cache or custom cache req */
  val builtIn = Bool()

  def isBuiltInType(): Bool = builtIn

  def isBuiltInType(t: Seq[Acquire.BuiltInTypeEnum.C]): Bool =
    builtIn && t.map(typ === _.asBits.asUInt).reduce(_ || _)

  def isPutAtomic: Bool = isBuiltInType(Seq(Acquire.BuiltInTypeEnum.PUT_ATOMIC))

  def isPut: Bool = isBuiltInType(
    Seq(Acquire.BuiltInTypeEnum.PUT, Acquire.BuiltInTypeEnum.PUT_BLOCK)
  )

  /** request type */
  val typ = UInt(
    widthOf(Acquire.BuiltInTypeEnum()) max log2Up(tlCoh.nAcquireTypes) bits
  )

  /** multiplex, write mask for put and put_block, memory op for put_atomic */
  val union: UInt = UInt(
    (tlWriteMaskBits max (tlByteAddrBits + MT_SZ + M_SZ)) bits
  )

  /** when put data, hint whether to allocate the block in any middle caches */
  val allocHint: Bool = Bool()

  /** op code for PutAtomic */
  def opCode: UInt = isPut ? M_XWR | union(0, M_SZ bits)

  /** op size for PutAtomic */
  def opSize: UInt = union(M_SZ, MT_SZ bits)

  /** byte address for PutAtomic operand */
  def byteAddr: UInt = union(M_SZ + MT_SZ, tlByteAddrBits bits)

  /** high bits of aligned amo address */
  private def byteAddrAlign: UInt = byteAddr >> log2Up(amoOperandBits / 8)

  /** amo bit offset of operand */
  def amoShiftBits: UInt = U(amoOperandBits) * byteAddrAlign

  /** write mask for put* */
  def byteMask: UInt = {

    /** put atomic, ...000111..111000..., decide by byte address */
    val atomicMask: UInt =
      FillInterleaved(amoOperandBits / 8, B(1) << byteAddrAlign).asUInt
        .resize(tlWriteMaskBits)

    /** put, decide by union */
    val putMask: UInt = union(0, tlWriteMaskBits bits)

    isPutAtomic ? atomicMask | (isPut ? putMask | U(0, tlWriteMaskBits bits))
  }

  def bitMask: UInt = FillInterleaved(8, byteMask.asBits).asUInt

  /** byte address for this request */
  def address: UInt = blockAddr @@ beatAddr @@ byteAddr

  /** type equality */
  def ===(that: Acquire): Bool = typ === that.typ

  def ===(that: UInt): Bool = typ === that

  /** is a built-in prefetch message */
  def isPrefetch: Bool = isBuiltInType(Seq(Acquire.BuiltInTypeEnum.PREFETCH))

  /** current xact has data */
  override def hasData: Bool =
    isBuiltInType() && Acquire.typesWithData
      .map(this === _.asBits.asUInt)
      .reduce(_ || _)

  /** current xact has multi beat data */
  override def hasDataMultiBeat: Bool =
    Bool(tlDataBeats > 1) && isBuiltInType() &&
      Acquire.typesWithMultiBeatData
        .map(this === _.asBits.asUInt)
        .reduce(_ || _)

  /** require manager to probe this client, when multiple client multiplex same port */
  def requireSelfProbe: Bool = False

  /** expect Grant for each built-in Acquire */
  def getBuiltInGrantType: Grant.BuiltInTypeEnum.C = typ.mux(
    Acquire.BuiltInTypeEnum.GET -> Grant.BuiltInTypeEnum.GET_ACK,
    Acquire.BuiltInTypeEnum.GET_BLOCK -> Grant.BuiltInTypeEnum.GET_BLOCK_ACK,
    Acquire.BuiltInTypeEnum.PUT -> Grant.BuiltInTypeEnum.PUT_ACK,
    Acquire.BuiltInTypeEnum.PUT_BLOCK -> Grant.BuiltInTypeEnum.PUT_ACK,
    Acquire.BuiltInTypeEnum.PUT_ATOMIC -> Grant.BuiltInTypeEnum.GET_ACK,
    Acquire.BuiltInTypeEnum.PREFETCH -> Grant.BuiltInTypeEnum.PREFETCH_ACK
  )
}

/** Contains definitions of the the built-in Acquire types and a factory
  * for [[Acquire]]
  *
  * In general you should avoid using this factory directly and use
  * [[ClientMetaData.makeAcquire]] for custom cached Acquires and
  * [[Get]], [[Put]], etc. for built-in uncached Acquires.
  */
object Acquire {

  /** built in types */
  object BuiltInTypeEnum extends SpinalEnum(binarySequential) {
    val GET, GET_BLOCK, PUT, PUT_BLOCK, PUT_ATOMIC, PREFETCH = newElement()
  }

  /** built in types that carry data */
  def typesWithData: Seq[BuiltInTypeEnum.C] =
    Seq(
      BuiltInTypeEnum.PUT,
      BuiltInTypeEnum.PUT_BLOCK,
      BuiltInTypeEnum.PUT_ATOMIC
    )

  /** built in types that carry multi beat data */
  def typesWithMultiBeatData: Seq[BuiltInTypeEnum.C] =
    Seq(
      BuiltInTypeEnum.PUT,
      BuiltInTypeEnum.PUT_BLOCK,
      BuiltInTypeEnum.PUT_ATOMIC
    )

  def fullWriteMask(implicit p: Parameters): UInt =
    UInt(new Acquire().tlWriteMaskBits bits).setAll()

  /** generic constructor */
  def apply(
      builtIn: Bool,
      typ: UInt,
      xactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt = U(0),
      data: UInt = U(0),
      union: UInt = U(0),
      allocHint: Bool = False
  )(implicit p: Parameters): Acquire = {
    val acq = new Acquire()
    acq.builtIn := builtIn
    acq.typ := typ
    acq.xactId := xactId
    acq.blockAddr := blockAddr
    acq.beatAddr := beatAddr
    acq.data := data
    acq.union := union
    acq.allocHint := allocHint
    acq
  }

  /** copy constructor */
  def apply(a: Acquire)(implicit p: Parameters): Acquire = {
    val acq = new Acquire()
    acq <> a
    acq
  }
}

/** get a single beat of data from outer memory
  * client can hint whether the block should be allocated in the middle levels
  */
object Get {

  /** get the whole beat */
  def apply(
      xactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      allocHint: Bool
  )(implicit
      p: Parameters
  ): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.GET.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      allocHint = allocHint,
      union = MT_Q @@ M_XRD
    )
  }

  /** get partial data of a beat */
  def apply(
      xactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      byteAddr: UInt,
      opSize: UInt,
      allocHint: Bool
  )(implicit
      p: Parameters
  ): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.GET.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      allocHint = allocHint,
      union = byteAddr @@ opSize.resize(MT_SZ) @@ M_XRD
    )
  }
}

/** Get a whole block from outer memory hierarchy */
object GetBlock {
  def apply(xactId: UInt, blockAddr: UInt, allocHint: Bool = True)(implicit
      p: Parameters
  ): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.GET_BLOCK.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      union = MT_Q @@ M_XRD,
      allocHint = allocHint
    )
  }
}

/** prefetch a block into outer memory with read permission */
object GetPrefetch {
  def apply(xactId: UInt, blockAddr: UInt)(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PREFETCH.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      union = MT_Q @@ M_XRD,
      allocHint = True
    )
  }
}

/** put a single beat into the outer memory */
object Put {
  def apply(
      xactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      data: UInt,
      mask: UInt
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      data = data,
      union = mask,
      allocHint = True
    )
  }
}

/** Put a whole cache block of data into the outer memory hierarchy
  * If the write mask is not full, the block will be allocated in the
  * next-outermost level of the hierarchy, will write to other word again?
  * If the write mask is full, the client can hint whether the block
  * should be allocated or not.
  */
object PutBlock {

  /** write partial data with mask */
  def apply(
      xactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      data: UInt,
      mask: UInt
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT_BLOCK.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      data = data,
      union = mask,
      allocHint = !mask.andR
    )
  }

  /** write a whole block */
  def apply(
      xactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      data: UInt,
      allocHint: Bool = True
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT_BLOCK.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      data = data,
      allocHint = allocHint,
      union = Acquire.fullWriteMask
    )
  }
}

/** prefetch a block into outer memory with write permission */
object PutPrefetch {
  def apply(xactId: UInt, blockAddr: UInt)(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PREFETCH.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      union = MT_Q @@ M_XWR,
      allocHint = True
    )
  }
}

/** Perform an atomic memory operation in the next-outermost level of the memory hierarchy
  */
object PutAtomic {
  def apply(
      xactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      byteAddr: UInt,
      data: UInt,
      opCode: UInt,
      opSize: UInt
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT_ATOMIC.asBits.asUInt,
      xactId = xactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      data = data,
      union = byteAddr @@ opSize @@ opCode,
      allocHint = True
    )
  }
}

object Grant {
  object BuiltInTypeEnum extends SpinalEnum(binarySequential) {
    val VOLUNTARY_ACK, PREFETCH_ACK, PUT_ACK, GET_ACK, GET_BLOCK_ACK =
      newElement()
  }
}
class TileLink {}
