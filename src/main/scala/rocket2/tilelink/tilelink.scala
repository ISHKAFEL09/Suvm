// See LICENSE for license details.

package rocket2.tilelink

import chisel3._
import chisel3.util._

import scala.math.max
import rocket2.config._
import MemoryOpConstants._
import rocket2._

/** Parameters exposed to the top-level design, set based on 
  * external requirements or design space exploration
  */
/** Unique name per TileLink network*/
case object TLId extends Field[String]
/** Coherency policy used to define custom mesage types */
case object TLCoherencePolicy extends Field[CoherencePolicy]
/** Number of manager agents */
case object TLNManagers extends Field[Int] 
/** Number of client agents */
case object TLNClients extends Field[Int]
/** Number of client agents that cache data and use custom [[uncore.Acquire]] types */
case object TLNCachingClients extends Field[Int]
/** Number of client agents that do not cache data and use built-in [[uncore.Acquire]] types */
case object TLNCachelessClients extends Field[Int]
/** Maximum number of unique outstanding transactions per client */
case object TLMaxClientXacts extends Field[Int]
/** Maximum number of clients multiplexed onto a single port */
case object TLMaxClientsPerPort extends Field[Int]
/** Maximum number of unique outstanding transactions per manager */
case object TLMaxManagerXacts extends Field[Int]
/** Width of cache block addresses */
case object TLBlockAddrBits extends Field[Int]
/** Width of data beats */
case object TLDataBits extends Field[Int]
/** Number of data beats per cache block */
case object TLDataBeats extends Field[Int]
/** Whether the underlying physical network preserved point-to-point ordering of messages */
case object TLNetworkIsOrderedP2P extends Field[Boolean]

/** Utility trait for building Modules and Bundles that use TileLink parameters */
trait TileLinkParameters {
  val p: Parameters
  val tlCoh = p(TLCoherencePolicy)
  val tlNManagers = p(TLNManagers)
  val tlNClients = p(TLNClients)
//  val tlNCachingClients = p(TLNCachingClients)
//  val tlNCachelessClients = p(TLNCachelessClients)
  val tlClientIdBits =  log2Up(tlNClients)
  val tlManagerIdBits =  log2Up(tlNManagers)
  val tlMaxClientXacts = p(TLMaxClientXacts)
  val tlMaxClientsPerPort = p(TLMaxClientsPerPort)
  val tlMaxManagerXacts = p(TLMaxManagerXacts)
  val tlClientXactIdBits = log2Up(tlMaxClientXacts*tlMaxClientsPerPort)
  val tlManagerXactIdBits = log2Up(tlMaxManagerXacts)
  val tlBlockAddrBits = p(TLBlockAddrBits)
  val tlDataBits = p(TLDataBits)
  val tlDataBytes = tlDataBits/8
  val tlDataBeats = p(TLDataBeats)
  val tlWriteMaskBits = if(tlDataBits/8 < 1) 1 else tlDataBits/8
  val tlBeatAddrBits = log2Up(tlDataBeats)
  val tlByteAddrBits = log2Up(tlWriteMaskBits)
  val tlMemoryOpcodeBits = M_SZ
  val tlMemoryOperandSizeBits = MT_SZ
  val tlAcquireTypeBits = max(log2Up(Acquire.nBuiltInTypes), 
                              tlCoh.acquireTypeWidth)
  val tlAcquireUnionBits = max(tlWriteMaskBits,
                                 (tlByteAddrBits +
                                   tlMemoryOperandSizeBits +
                                   tlMemoryOpcodeBits)) + 1
  val tlGrantTypeBits = max(log2Up(Grant.nBuiltInTypes), 
                              tlCoh.grantTypeWidth) + 1
  val tlNetworkPreservesPointToPointOrdering = p(TLNetworkIsOrderedP2P)
  val tlNetworkDoesNotInterleaveBeats = true
  val amoAluOperandBits = p(AmoAluOperandBits)
}

trait TLBundle extends Bundle with TileLinkParameters
trait TLModule extends Module with TileLinkParameters

/** Base trait for all TileLink channels */
trait TileLinkChannel extends TLBundle {
  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
trait ClientToManagerChannel extends TileLinkChannel
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
trait ManagerToClientChannel extends TileLinkChannel
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
trait ClientToClientChannel extends TileLinkChannel // Unused for now

/** Common signals that are used in multiple channels.
  * These traits are useful for type parameterizing bundle wiring functions.
  */

/** Address of a cache block. */
trait HasCacheBlockAddress extends TLBundle {
  val addr_block = UInt(tlBlockAddrBits.W)

  def conflicts(that: HasCacheBlockAddress) = this.addr_block === that.addr_block
  def conflicts(addr: UInt) = this.addr_block === addr
}

/** Sub-block address or beat id of multi-beat data */
trait HasTileLinkBeatId extends TLBundle {
  val addr_beat = UInt(tlBeatAddrBits.W)
}

/* Client-side transaction id. Usually Miss Status Handling Register File index */
trait HasClientTransactionId extends TLBundle {
  val client_xact_id = UInt(tlClientXactIdBits.W)
}

/** Manager-side transaction id. Usually Transaction Status Handling Register File index. */
trait HasManagerTransactionId extends TLBundle {
  val manager_xact_id = UInt(tlManagerXactIdBits.W)
}

/** A single beat of cache block data */
trait HasTileLinkData extends HasTileLinkBeatId {
  val data = UInt(tlDataBits.W)

  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}

/** The id of a client source or destination. Used in managers. */
trait HasClientId extends TLBundle {
  val client_id = UInt(tlClientIdBits.W)
}

/** TileLink channel bundle definitions */

/** The Acquire channel is used to intiate coherence protocol transactions in
  * order to gain access to a cache block's data with certain permissions
  * enabled. Messages sent over this channel may be custom types defined by
  * a [[uncore.CoherencePolicy]] for cached data accesse or may be built-in types
  * used for uncached data accesses. Acquires may contain data for Put or
  * PutAtomic built-in types. After sending an Acquire, clients must
  * wait for a manager to send them a [[uncore.Grant]] message in response.
  */
class Acquire(implicit val p: Parameters) extends ClientToManagerChannel 
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  // Actual bundle fields:
  val is_builtin_type = Bool()
  val a_type = UInt(tlAcquireTypeBits.W)
  val union = UInt(tlAcquireUnionBits.W)

  // Utility funcs for accessing subblock union:
  val opCodeOff = 1
  val opSizeOff = tlMemoryOpcodeBits + opCodeOff
  val addrByteOff = tlMemoryOperandSizeBits + opSizeOff
  val addrByteMSB = tlByteAddrBits + addrByteOff
  /** Hint whether to allocate the block in any interveneing caches */
  def allocate(dummy: Int = 0) = union(0)
  /** Op code for [[uncore.PutAtomic]] operations */
  def op_code(dummy: Int = 0) = Mux[UInt](
    isBuiltInType(Acquire.putType) || isBuiltInType(Acquire.putBlockType),
    M_XWR, union(opSizeOff-1, opCodeOff))
  /** Operand size for [[uncore.PutAtomic]] */
  def op_size(dummy: Int = 0) = union(addrByteOff-1, opSizeOff)
  /** Byte address for [[uncore.PutAtomic]] operand */
  def addr_byte(dummy: Int = 0) = union(addrByteMSB-1, addrByteOff)
  private def amo_offset(dummy: Int = 0) = addr_byte()(tlByteAddrBits-1, log2Up(amoAluOperandBits/8))
  /** Bit offset of [[uncore.PutAtomic]] operand */
  def amo_shift_bits(dummy: Int = 0) = UInt(amoAluOperandBits.W)*amo_offset()
  /** Write mask for [[uncore.Put]], [[uncore.PutBlock]], [[uncore.PutAtomic]] */
  def wmask(dummy: Int = 0) = 
    Mux(isBuiltInType(Acquire.putAtomicType), 
      FillInterleaved(amoAluOperandBits/8, UIntToOH(amo_offset())),
      Mux(isBuiltInType(Acquire.putBlockType) || isBuiltInType(Acquire.putType),
        union(tlWriteMaskBits, 1),
        0.U(tlWriteMaskBits.W)))
  /** Full, beat-sized writemask */
  def full_wmask(dummy: Int = 0) = FillInterleaved(8, wmask())
  /** Complete physical address for block, beat or operand */
  def addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, this.addr_byte())

  // Other helper functions:
  /** Message type equality */
  def is(t: UInt) = a_type === t //TODO: make this more opaque; def ===?

  /** Is this message a built-in or custom type */
  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  /** Is this message a particular built-in type */
  def isBuiltInType(t: UInt): Bool = is_builtin_type && a_type === t 

  /** Does this message refer to subblock operands using info in the Acquire.union subbundle */ 
  def isSubBlockType(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesOnSubBlocks.contains(a_type) 

  /** Is this message a built-in prefetch message */
  def isPrefetch(dummy: Int = 0): Bool = isBuiltInType() && is(Acquire.prefetchType) 

  /** Does this message contain data? Assumes that no custom message types have data. */
  def hasData(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesWithData.contains(a_type)

  /** Does this message contain multiple beats of data? Assumes that no custom message types have data. */
  def hasMultibeatData(dummy: Int = 0): Bool = (tlDataBeats > 1).B && isBuiltInType() &&
                                           Acquire.typesWithMultibeatData.contains(a_type)

  /** Does this message require the manager to probe the client the very client that sent it?
    * Needed if multiple caches are attached to the same port.
    */
  def requiresSelfProbe(dummy: Int = 0) = false.B

  /** Mapping between each built-in Acquire type (defined in companion object)
    * and a built-in Grant type.
    */
  def getBuiltInGrantType(dummy: Int = 0): UInt = {
    MuxLookup(this.a_type, Grant.putAckType, Seq(
      Acquire.getType       -> Grant.getDataBeatType,
      Acquire.getBlockType  -> Grant.getDataBlockType,
      Acquire.putType       -> Grant.putAckType,
      Acquire.putBlockType  -> Grant.putAckType,
      Acquire.putAtomicType -> Grant.getDataBeatType,
      Acquire.prefetchType  -> Grant.prefetchAckType))
  }
}

/** [[uncore.Acquire]] with an extra field stating its source id */
class AcquireFromSrc(implicit p: Parameters) extends Acquire with HasClientId

/** Contains definitions of the the built-in Acquire types and a factory
  * for [[uncore.Acquire]]
  *
  * In general you should avoid using this factory directly and use
  * [[uncore.ClientMetadata.makeAcquire]] for custom cached Acquires and
  * [[uncore.Get]], [[uncore.Put]], etc. for built-in uncached Acquires.
  *
  * @param is_builtin_type built-in or custom type message?
  * @param a_type built-in type enum or custom type enum
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat)
  * @param data data being put outwards
  * @param union additional fields used for uncached types
  */
object Acquire {
  val nBuiltInTypes = 5
  //TODO: Use Enum
  def getType       = "b000".U // Get a single beat of data
  def getBlockType  = "b001".U // Get a whole block of data
  def putType       = "b010".U // Put a single beat of data
  def putBlockType  = "b011".U // Put a whole block of data
  def putAtomicType = "b100".U // Perform an atomic memory op
  def prefetchType  = "b101".U // Prefetch a whole block of data
  def typesWithData = VecInit(putType, putBlockType, putAtomicType)
  def typesWithMultibeatData = VecInit(putBlockType)
  def typesOnSubBlocks = VecInit(putType, getType, putAtomicType)

  def fullWriteMask(implicit p: Parameters) = (-1).S(new Acquire().tlWriteMaskBits.W).asUInt

  // Most generic constructor
  def apply(
      is_builtin_type: Bool,
      a_type: Bits,
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = UInt(0.W),
      data: UInt = UInt(0.W),
      union: UInt = UInt(0.W))(implicit p: Parameters): Acquire = {
    val acq = new Acquire
    acq.is_builtin_type := is_builtin_type
    acq.a_type := a_type
    acq.client_xact_id := client_xact_id
    acq.addr_block := addr_block
    acq.addr_beat := addr_beat
    acq.data := data
    acq.union := union
    acq
  }
  // Copy constructor
  def apply(a: Acquire)(implicit p: Parameters): Acquire = {
    val acq = new Acquire
    acq := a
    acq
  }
}

/** The Probe channel is used to force clients to release data or cede permissions
  * on a cache block. Clients respond to Probes with [[uncore.Release]] messages.
  * The available types of Probes are customized by a particular
  * [[uncore.CoherencePolicy]].
  */
class Probe(implicit val p: Parameters) extends ManagerToClientChannel 
    with HasCacheBlockAddress {
  val p_type = UInt(tlCoh.probeTypeWidth.W)

  def is(t: UInt) = p_type === t
  def hasData(dummy: Int = 0) = false.B
  def hasMultibeatData(dummy: Int = 0) = false.B
}

/** [[uncore.Probe]] with an extra field stating its destination id */
class ProbeToDst(implicit p: Parameters) extends Probe with HasClientId

/** Contains factories for [[uncore.Probe]] and [[uncore.ProbeToDst]]
  *
  * In general you should avoid using these factories directly and use
  * [[uncore.ManagerMetadata.makeProbe(UInt,Acquire)* makeProbe]] instead.
  *
  * @param dst id of client to which probe should be sent
  * @param p_type custom probe type
  * @param addr_block address of the cache block
  */
object Probe {
  def apply(p_type: UInt, addr_block: UInt)(implicit p: Parameters): Probe = {
    val prb = new Probe
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
  def apply(dst: UInt, p_type: UInt, addr_block: UInt)(implicit p: Parameters): ProbeToDst = {
    val prb = new ProbeToDst
    prb.client_id := dst
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
}

/** The Release channel is used to release data or permission back to the manager
  * in response to [[uncore.Probe]] messages. It can also be used to voluntarily
  * write back data, for example in the event that dirty data must be evicted on
  * a cache miss. The available types of Release messages are always customized by
  * a particular [[uncore.CoherencePolicy]]. Releases may contain data or may be
  * simple acknowledgements. Voluntary Releases are acknowledged with [[uncore.Grant Grants]].
  */
class Release(implicit val p: Parameters) extends ClientToManagerChannel 
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  val r_type = UInt(tlCoh.releaseTypeWidth.W)
  val voluntary = Bool()

  // Helper funcs
  def is(t: UInt) = r_type === t
  def hasData(dummy: Int = 0) = tlCoh.releaseTypesWithData.contains(r_type)
  //TODO: Assumes all releases write back full cache blocks:
  def hasMultibeatData(dummy: Int = 0) = (tlDataBeats > 1).B && tlCoh.releaseTypesWithData.contains(r_type)
  def isVoluntary(dummy: Int = 0) = voluntary
  def requiresAck(dummy: Int = 0) = !tlNetworkPreservesPointToPointOrdering.B
}

/** [[uncore.Release]] with an extra field stating its source id */
class ReleaseFromSrc(implicit p: Parameters) extends Release with HasClientId

/** Contains a [[uncore.Release]] factory
  *
  * In general you should avoid using this factory directly and use
  * [[uncore.ClientMetadata.makeRelease]] instead.
  *
  * @param voluntary is this a voluntary writeback
  * @param r_type type enum defined by coherence protocol
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat beat id of the data
  * @param data data being written back
  */
object Release {
  def apply(
      voluntary: Bool,
      r_type: UInt,
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = 0.U,
      data: UInt = 0.U)(implicit p: Parameters): Release = {
    val rel = new Release
    rel.r_type := r_type
    rel.client_xact_id := client_xact_id
    rel.addr_block := addr_block
    rel.addr_beat := addr_beat
    rel.data := data
    rel.voluntary := voluntary
    rel
  }
}

/** The Grant channel is used to refill data or grant permissions requested of the 
  * manager agent via an [[uncore.Acquire]] message. It is also used to acknowledge
  * the receipt of voluntary writeback from clients in the form of [[uncore.Release]]
  * messages. There are built-in Grant messages used for Gets and Puts, and
  * coherence policies may also define custom Grant types. Grants may contain data
  * or may be simple acknowledgements. Grants are responded to with [[uncore.Finish]].
  */
class Grant(implicit val p: Parameters) extends ManagerToClientChannel
    with HasTileLinkData 
    with HasClientTransactionId 
    with HasManagerTransactionId {
  val is_builtin_type = Bool()
  val g_type = UInt(tlGrantTypeBits.W)

  // Helper funcs
  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  def isBuiltInType(t: UInt): Bool = is_builtin_type && g_type === t 
  def is(t: UInt):Bool = g_type === t
  def hasData(dummy: Int = 0): Bool = Mux(isBuiltInType(),
                                        Grant.typesWithData.contains(g_type),
                                        tlCoh.grantTypesWithData.contains(g_type))
  def hasMultibeatData(dummy: Int = 0): Bool = 
    (tlDataBeats > 1).B && Mux(isBuiltInType(),
                               Grant.typesWithMultibeatData.contains(g_type),
                               tlCoh.grantTypesWithData.contains(g_type))
  def isVoluntary(dummy: Int = 0): Bool = isBuiltInType() && (g_type === Grant.voluntaryAckType)
  def requiresAck(dummy: Int = 0): Bool = !tlNetworkPreservesPointToPointOrdering.B && !isVoluntary()
  def makeFinish(dummy: Int = 0): Finish = {
    val f = Wire(new Finish)
    f.manager_xact_id := this.manager_xact_id
    f
  }
}

/** [[uncore.Grant]] with an extra field stating its destination */
class GrantToDst(implicit p: Parameters) extends Grant with HasClientId

/** Contains definitions of the the built-in grant types and factories
  * for [[uncore.Grant]] and [[uncore.GrantToDst]]
  *
  * In general you should avoid using these factories directly and use
  * [[uncore.ManagerMetadata.makeGrant(uncore.AcquireFromSrc* makeGrant]] instead.
  *
  * @param dst id of client to which grant should be sent
  * @param is_builtin_type built-in or custom type message?
  * @param g_type built-in type enum or custom type enum
  * @param client_xact_id client's transaction id
  * @param manager_xact_id manager's transaction id
  * @param addr_beat beat id of the data
  * @param data data being refilled to the original requestor
  */
object Grant {
  val nBuiltInTypes = 5
  def voluntaryAckType = "b000".U // For acking Releases
  def prefetchAckType  = "b001".U // For acking any kind of Prefetch
  def putAckType       = "b011".U // For acking any kind of non-prfetch Put
  def getDataBeatType  = "b100".U // Supplying a single beat of Get
  def getDataBlockType = ("b101").U // Supplying all beats of a GetBlock
  def typesWithData = VecInit(getDataBlockType, getDataBeatType)
  def typesWithMultibeatData= VecInit(getDataBlockType)

  def apply(
      is_builtin_type: Bool,
      g_type: UInt,
      client_xact_id: UInt, 
      manager_xact_id: UInt,
      addr_beat: UInt,
      data: UInt)(implicit p: Parameters): Grant = {
    val gnt = new Grant
    gnt.is_builtin_type := is_builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }

  def apply(
      dst: UInt,
      is_builtin_type: Bool,
      g_type: UInt,
      client_xact_id: UInt,
      manager_xact_id: UInt,
      addr_beat: UInt = 0.U,
      data: UInt = 0.U)(implicit p: Parameters): GrantToDst = {
    val gnt = new GrantToDst
    gnt.client_id := dst
    gnt.is_builtin_type := is_builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }
}

/** The Finish channel is used to provide a global ordering of transactions
  * in networks that do not guarantee point-to-point ordering of messages.
  * A Finsish message is sent as acknowledgement of receipt of a [[uncore.Grant]].
  * When a Finish message is received, a manager knows it is safe to begin
  * processing other transactions that touch the same cache block.
  */
class Finish(implicit val p: Parameters) extends ClientToManagerChannel with HasManagerTransactionId {
  def hasData(dummy: Int = 0) = false.B
  def hasMultibeatData(dummy: Int = 0) = false.B
}

class ClientUncachedTileLinkIO(implicit val p: Parameters) extends TLBundle {
  val acquire   = new DecoupledIO(new Acquire)
  val grant     = Flipped(new DecoupledIO(new Grant))
}

/** This version of TileLinkIO does not contain network headers.
 * It is intended for use within client agents.
 */
class ClientTileLinkIO(implicit p: Parameters) extends ClientUncachedTileLinkIO {
  val probe     = Flipped(new DecoupledIO(new Probe))
  val release   = new DecoupledIO(new Release)
}