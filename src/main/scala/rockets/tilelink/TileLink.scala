package rockets.tilelink

import rockets.core.consts.MemOps._
import rockets.params._
import spinal.core._
import spinal.lib._
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
  val clientXactId = UInt(tlClientXactIdBits bits)
}

/** manager side xact id? */
trait HasTLManagerXactId extends TLBundle {
  val managerXactId = UInt(tlManagerXactIdBits bits)
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

  def isBuiltInType: Bool = builtIn

  def isBuiltInType(t: Acquire.BuiltInTypeEnum.C): Bool =
    builtIn && this === t

  def isPutAtomic: Bool = isBuiltInType(Acquire.BuiltInTypeEnum.PUT_ATOMIC)

  def isPut: Bool =
    isBuiltInType(Acquire.BuiltInTypeEnum.PUT) ||
      isBuiltInType(Acquire.BuiltInTypeEnum.PUT_BLOCK)

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

  def ===(that: Acquire.BuiltInTypeEnum.C): Bool = typ === that.asUInt

  /** is a built-in prefetch message */
  def isPrefetch: Bool = isBuiltInType(Acquire.BuiltInTypeEnum.PREFETCH)

  /** current xact has data */
  override def hasData: Bool =
    isBuiltInType && Acquire.typesWithData
      .map(this === _.asUInt)
      .reduce(_ || _)

  /** current xact has multi beat data */
  override def hasDataMultiBeat: Bool =
    Bool(tlDataBeats > 1) && isBuiltInType &&
      Acquire.typesWithMultiBeatData
        .map(this === _.asUInt)
        .reduce(_ || _)

  /** require manager to probe this client, when multiple client multiplex same port */
  def requireSelfProbe: Bool = False

  /** expect Grant for each built-in Acquire */
  def getBuiltInGrantType: Grant.BuiltInTypeEnum.C = typ.mux(
    Acquire.BuiltInTypeEnum.GET.asUInt -> Grant.BuiltInTypeEnum.GET_ACK,
    Acquire.BuiltInTypeEnum.GET_BLOCK.asUInt -> Grant.BuiltInTypeEnum.GET_BLOCK_ACK,
    Acquire.BuiltInTypeEnum.PUT.asUInt -> Grant.BuiltInTypeEnum.PUT_ACK,
    Acquire.BuiltInTypeEnum.PUT_BLOCK.asUInt -> Grant.BuiltInTypeEnum.PUT_ACK,
    Acquire.BuiltInTypeEnum.PUT_ATOMIC.asUInt -> Grant.BuiltInTypeEnum.GET_ACK,
    Acquire.BuiltInTypeEnum.PREFETCH.asUInt -> Grant.BuiltInTypeEnum.PREFETCH_ACK,
    default -> Grant.BuiltInTypeEnum.GET_ACK
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
      clientXactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt = U(0),
      data: UInt = U(0),
      union: UInt = U(0),
      allocHint: Bool = False
  )(implicit p: Parameters): Acquire = {
    val acq = new Acquire()
    acq.builtIn := builtIn
    acq.typ := typ
    acq.clientXactId := clientXactId
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
      clientXactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      allocHint: Bool
  )(implicit
      p: Parameters
  ): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.GET.asUInt,
      clientXactId = clientXactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      allocHint = allocHint,
      union = MT_Q @@ M_XRD
    )
  }

  /** get partial data of a beat */
  def apply(
      clientXactId: UInt,
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
      typ = Acquire.BuiltInTypeEnum.GET.asUInt,
      clientXactId = clientXactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      allocHint = allocHint,
      union = byteAddr @@ opSize.resize(MT_SZ) @@ M_XRD
    )
  }
}

/** Get a whole block from outer memory hierarchy */
object GetBlock {
  def apply(clientXactId: UInt, blockAddr: UInt, allocHint: Bool = True)(
      implicit p: Parameters
  ): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.GET_BLOCK.asUInt,
      clientXactId = clientXactId,
      blockAddr = blockAddr,
      union = MT_Q @@ M_XRD,
      allocHint = allocHint
    )
  }
}

/** prefetch a block into outer memory with read permission */
object GetPrefetch {
  def apply(clientXactId: UInt, blockAddr: UInt)(implicit
      p: Parameters
  ): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PREFETCH.asUInt,
      clientXactId = clientXactId,
      blockAddr = blockAddr,
      union = MT_Q @@ M_XRD,
      allocHint = True
    )
  }
}

/** put a single beat into the outer memory */
object Put {
  def apply(
      clientXactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      data: UInt,
      mask: UInt
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT.asUInt,
      clientXactId = clientXactId,
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
      clientXactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      data: UInt,
      mask: UInt
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT_BLOCK.asUInt,
      clientXactId = clientXactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      data = data,
      union = mask,
      allocHint = !mask.andR
    )
  }

  /** write a whole block */
  def apply(
      clientXactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      data: UInt,
      allocHint: Bool = True
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT_BLOCK.asUInt,
      clientXactId = clientXactId,
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
  def apply(clientXactId: UInt, blockAddr: UInt)(implicit
      p: Parameters
  ): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PREFETCH.asUInt,
      clientXactId = clientXactId,
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
      clientXactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      byteAddr: UInt,
      data: UInt,
      opCode: UInt,
      opSize: UInt
  )(implicit p: Parameters): Acquire = {
    Acquire(
      builtIn = True,
      typ = Acquire.BuiltInTypeEnum.PUT_ATOMIC.asUInt,
      clientXactId = clientXactId,
      blockAddr = blockAddr,
      beatAddr = beatAddr,
      data = data,
      union = byteAddr @@ opSize @@ opCode,
      allocHint = True
    )
  }
}

/** The Probe channel is used to force clients to release data or give up permissions
  * on a cache block.
  */
class Probe(implicit p: Parameters) extends M2CChannel with HasTLBlockAddress {
  val typ = UInt(log2Up(tlCoh.nProbeTypes) bits)

  /** current xact has data */
  override def hasData: Bool = ???

  /** current xact has multi beat data */
  override def hasDataMultiBeat: Bool = ???
}

/** Probe to particular client */
class ProbeWithCid(implicit p: Parameters) extends Probe with HasTLClientId

object Probe {
  def apply(typ: UInt, blockAddr: UInt)(implicit p: Parameters): Probe = {
    val probe = new Probe()
    probe.typ := typ
    probe.blockAddr := blockAddr
    probe
  }

  def apply(clientId: UInt, typ: UInt, blockAddr: UInt)(implicit
      p: Parameters
  ): Probe = {
    val probe = new ProbeWithCid()
    probe.typ := typ
    probe.blockAddr := blockAddr
    probe.clientId := clientId
    probe
  }
}

/** The Release channel is used to release data or permission back to the manager
  * in response to [[Probe]] messages. It can also be used to voluntarily
  * write back data, for example in the event that dirty data must be evicted on
  * a cache miss. The available types of Release messages are always customized by
  * a particular [[CoherencePolicy]]. Releases may contain data or may be
  * simple acknowledgements. Voluntary Releases are acknowledged with [[Grant Grants]].
  */
class Release(implicit p: Parameters)
    extends C2MChannel
    with HasTLBlockAddress
    with HasTLClientXactId
    with HasTLData {
  val typ = UInt(log2Up(tlCoh.nReleaseTypes) bits)

  /** whether is voluntary release or response to Probe */
  val voluntary = Bool()

  def ===(that: UInt): Bool = typ === that

  /** current xact has data */
  override def hasData: Bool = tlCoh.releaseTypesWithData.sContains(typ)

  /** current xact has multi beat data */
  override def hasDataMultiBeat: Bool = hasData && Bool(tlDataBeats > 1)

  def isVoluntary: Bool = voluntary
}

/** [[Release]] with an extra field stating source id */
class ReleaseWithCid(implicit p: Parameters) extends Release with HasTLClientId

/** [[Release]] factory */
object Release {
  def apply(
      voluntary: Bool,
      typ: UInt,
      clientXactId: UInt,
      blockAddr: UInt,
      beatAddr: UInt,
      data: UInt
  )(implicit p: Parameters): Release = {
    val r = new Release()
    r.typ := typ
    r.voluntary := voluntary
    r.clientXactId := clientXactId
    r.blockAddr := blockAddr
    r.beatAddr := beatAddr
    r.data := data
    r
  }
}

/** The Grant channel is used to refill data or grant permissions requested of the
  * manager agent via an [[Acquire]] message. It is also used to acknowledge
  * the receipt of voluntary write back from clients in the form of [[Release]]
  * messages. There are built-in Grant messages used for Gets and Puts, and
  * coherence policies may also define custom Grant types. Grants may contain data
  * or may be simple acknowledgements. Grants are responded to with [[Finish]].
  */
class Grant(implicit p: Parameters)
    extends M2CChannel
    with HasTLData
    with HasTLClientXactId
    with HasTLManagerXactId {
  val builtIn = Bool()

  val typ = UInt(
    widthOf(Grant.BuiltInTypeEnum()) max log2Up(tlCoh.nGrantTypes) bits
  )

  def isBuiltInType: Bool = builtIn

  def isBuiltInType(t: Grant.BuiltInTypeEnum.C): Bool =
    builtIn && this === t

  /** type equality */
  def ===(that: UInt): Bool = typ === that

  def ===(that: Grant.BuiltInTypeEnum.C): Bool = typ === that.asUInt

  /** current xact has data */
  override def hasData: Bool =
    isBuiltInType ?
      Grant.typesWithData.map(this === _).reduce(_ || _) |
      tlCoh.grantTypesWithData.sContains(typ)

  /** current xact has multi beat data */
  override def hasDataMultiBeat: Bool =
    Bool(tlDataBeats > 1) && (isBuiltInType ?
      Grant.typesWithMultiBeatData.map(this === _).reduce(_ || _) |
      tlCoh.grantTypesWithData.sContains(typ))

  def isVoluntary: Bool =
    isBuiltInType && this === Grant.BuiltInTypeEnum.VOLUNTARY_ACK
}

/** [[Grant]] with dest client id */
class GrantWithCid(implicit p: Parameters) extends Grant with HasTLClientId

/** [[Grant]] factory */
object Grant {
  object BuiltInTypeEnum extends SpinalEnum(binarySequential) {
    val VOLUNTARY_ACK, PREFETCH_ACK, PUT_ACK, GET_ACK, GET_BLOCK_ACK =
      newElement()
  }

  val typesWithData: Seq[BuiltInTypeEnum.C] =
    Seq(BuiltInTypeEnum.GET_BLOCK_ACK, BuiltInTypeEnum.GET_ACK)

  val typesWithMultiBeatData: Seq[BuiltInTypeEnum.C] = Seq(
    BuiltInTypeEnum.GET_BLOCK_ACK
  )

  def apply(
      builtIn: Bool,
      typ: UInt,
      clientXactId: UInt,
      managerXactId: UInt,
      beatAddr: UInt,
      data: UInt
  )(implicit p: Parameters): Grant = {
    val g = new Grant
    g.builtIn := builtIn
    g.typ := typ
    g.clientXactId := clientXactId
    g.managerXactId := managerXactId
    g.beatAddr := beatAddr
    g.data := data
    g
  }

  def apply(
      dest: UInt,
      builtIn: Bool,
      typ: UInt,
      clientXactId: UInt,
      managerXactId: UInt,
      beatAddr: UInt,
      data: UInt
  )(implicit p: Parameters): GrantWithCid = {
    val g = new GrantWithCid()
    g.builtIn := builtIn
    g.typ := typ
    g.clientXactId := clientXactId
    g.managerXactId := managerXactId
    g.beatAddr := beatAddr
    g.data := data
    g.clientId := dest
    g
  }
}

/** The Finish channel is used to provide a global ordering of transactions
  * in networks that do not guarantee point-to-point ordering of messages.
  * A Finsish message is sent as acknowledgement of receipt of a [[Grant]].
  * When a Finish message is received, a manager knows it is safe to begin
  * processing other transactions that touch the same cache block.
  */
class Finish(implicit p: Parameters)
    extends C2MChannel
    with HasTLManagerXactId {

  /** current xact has data */
  override def hasData: Bool = False

  /** current xact has multi beat data */
  override def hasDataMultiBeat: Bool = False
}
