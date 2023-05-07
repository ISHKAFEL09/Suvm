// See LICENSE for license details.

package rocket2.tilelink

import chisel3._
import chisel3.util._
import rocket2._
import rocket2.config._
import MemoryOpConstants._

/** Base class to represent coherence information in clients and managers */
abstract class CoherenceMetadata extends Bundle {
  implicit val p: Parameters
  val co = p(TLCoherencePolicy)
  val id = p(TLId)
}

/** Stores the client-side coherence information,
  * such as permissions on the data and whether the data is dirty.
  * Its API can be used to make TileLink messages in response to
  * memory operations or [[uncore.Probe]] messages.
  */
class ClientMetadata(implicit val p: Parameters) extends CoherenceMetadata {
  /** Actual state information stored in this bundle */
  val state = UInt(co.clientStateWidth.W)

  /** Metadata equality */
  def ===(rhs: ClientMetadata): Bool = this.state === rhs.state
  def !=(rhs: ClientMetadata): Bool = !this.===(rhs)

  /** Is the block's data present in this cache */
  def isValid(dummy: Int = 0): Bool = co.isValid(this)
  /** Does this cache have permissions on this block sufficient to perform op */
  def isHit(op_code: UInt): Bool = co.isHit(op_code, this)
  /** Does this cache lack permissions on this block sufficient to perform op */ 
  def isMiss(op_code: UInt): Bool = !co.isHit(op_code, this)
  /** Does a secondary miss on the block require another Acquire message */
  def requiresAcquireOnSecondaryMiss(first_op: UInt, second_op: UInt): Bool =
    co.requiresAcquireOnSecondaryMiss(first_op, second_op, this)
  /** Does op require a Release to be made to outer memory */
  def requiresReleaseOnCacheControl(op_code: UInt): Bool =
    co.requiresReleaseOnCacheControl(op_code: UInt, this)
  /** Does an eviction require a Release to be made to outer memory */
  def requiresVoluntaryWriteback(dummy: Int = 0): Bool =
    co.requiresReleaseOnCacheControl(M_FLUSH, this)

  /** Constructs an Acquire message based on this metdata and a memory operation
    *
    * @param client_xact_id client's transaction id
    * @param addr_block address of the cache block
    * @param op_code a memory operation from [[uncore.constants.MemoryOpConstants]]
    */
  def makeAcquire(
      client_xact_id: UInt,
      addr_block: UInt,
      op_code: UInt): Acquire = {
    Wire(Acquire(
        is_builtin_type = false.B,
        a_type = co.getAcquireType(op_code, this),
        client_xact_id = client_xact_id,
        addr_block = addr_block,
        union = Cat(op_code, true.B)))
  }

  /** Constructs a Release message based on this metadata on an eviction
    *
    * @param client_xact_id client's transaction id
    * @param addr_block address of the cache block
    * @param addr_beat sub-block address (which beat)
    * @param data data being written back
    */
  def makeVoluntaryWriteback(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = 0.U,
      data: UInt = 0.U): Release = {
    Wire(Release(
      voluntary = true.B,
      r_type = co.getReleaseType(M_FLUSH, this),
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data))
  }

  /** Constructs a Release message based on this metadata and a [[uncore.Probe]]
    *
    * @param the incoming [[uncore.Probe]]
    * @param addr_beat sub-block address (which beat)
    * @param data data being released
    */
  def makeRelease(
      prb: Probe,
      addr_beat: UInt = 0.U,
      data: UInt = 0.U): Release = {
    Wire(Release(
      voluntary = false.B,
      r_type = co.getReleaseType(prb, this),
      client_xact_id = 0.U,
      addr_block = prb.addr_block,
      addr_beat = addr_beat,
      data = data))
  }

  /** New metadata after receiving a [[uncore.Grant]]
    *
    * @param incoming the incoming [[uncore.Grant]]
    * @param pending the mem op that triggered this transaction
    */
  def onGrant(incoming: Grant, pending: UInt): ClientMetadata =
    co.clientMetadataOnGrant(incoming, pending, this)

  /** New metadata after receiving a [[uncore.Probe]]
    *
    * @param incoming the incoming [[uncore.Probe]]
    */
  def onProbe(incoming: Probe): ClientMetadata =
    co.clientMetadataOnProbe(incoming, this)

  /** New metadata after a op_code hits this block
    *
    * @param op_code a memory operation from [[uncore.constants.MemoryOpConstants]]
    */
  def onHit(op_code: UInt): ClientMetadata =
    co.clientMetadataOnHit(op_code, this)

  /** New metadata after receiving a [[uncore.Probe]]
    *
    * @param op_code a memory operation from [[uncore.constants.MemoryOpConstants]]
    */
  def onCacheControl(op_code: UInt): ClientMetadata =
    Wire(co.clientMetadataOnCacheControl(op_code, this))
}

/** Factories for ClientMetadata, including on reset */
object ClientMetadata {
  def apply(state: UInt)(implicit p: Parameters) = {
    val meta = new Bundle {val meta = new ClientMetadata}
    meta.meta.state := state
    meta.meta
  }
  def onReset(implicit p: Parameters) = new ClientMetadata().co.clientMetadataOnReset
}

/** Stores manager-side information about the status 
  * of a cache block, including whether it has any known sharers.
  *
  * Its API can be used to create [[uncore.Probe]] and [[uncore.Grant]] messages.
  */
class ManagerMetadata(implicit val p: Parameters) extends CoherenceMetadata {
  // Currently no coherence policies assume manager-side state information
  // val state = UInt(width = co.masterStateWidth) TODO: Fix 0-width wires in Chisel

  /** The directory information for this block */
  val sharers = UInt(width = co.dir.width.W)

  /** Metadata equality */
  def ===(rhs: ManagerMetadata): Bool = //this.state === rhs.state && TODO: Fix 0-width wires in Chisel
                                         this.sharers === rhs.sharers
  def !=(rhs: ManagerMetadata): Bool = !this.===(rhs)

  /** Converts the directory info into an N-hot sharer bitvector (i.e. full representation) */
  def full(dummy: Int = 0): UInt = co.dir.full(this.sharers)

  /** Does this [[uncore.Acquire]] require [[uncore.Probe Probes]] to be sent */
  def requiresProbes(acq: Acquire): Bool = co.requiresProbes(acq, this)
  /** Does this memory op require [[uncore.Probe Probes]] to be sent */
  def requiresProbes(op_code: UInt): Bool = co.requiresProbes(op_code, this)
  /** Does an eviction require [[uncore.Probe Probes]] to be sent */
  def requiresProbesOnVoluntaryWriteback(dummy: Int = 0): Bool =
    co.requiresProbes(M_FLUSH, this)

  /** Construct an appropriate [[uncore.ProbeToDst]] for a given [[uncore.Acquire]]
    *
    * @param dst Destination client id for this Probe
    * @param acq Acquire message triggering this Probe
    */
  def makeProbe(dst: UInt, acq: Acquire): ProbeToDst = 
    Wire(Probe(dst, co.getProbeType(acq, this), acq.addr_block))

  /** Construct an appropriate [[uncore.ProbeToDst]] for a given mem op
    *
    * @param dst Destination client id for this Probe
    * @param op_code memory operation triggering this Probe
    * @param addr_block address of the cache block being probed
    */
  def makeProbe(dst: UInt, op_code: UInt, addr_block: UInt): ProbeToDst =
    Wire(Probe(dst, co.getProbeType(op_code, this), addr_block))

  /** Construct an appropriate [[uncore.ProbeToDst]] for an eviction
    *
    * @param dst Destination client id for this Probe
    * @param addr_block address of the cache block being probed prior to eviction
    */
  def makeProbeForVoluntaryWriteback(dst: UInt, addr_block: UInt): ProbeToDst =
    makeProbe(dst, M_FLUSH, addr_block)

  /** Construct an appropriate [[uncore.GrantToDst]] to acknowledge an [[uncore.Release]]
    *
    * @param rel Release message being acknowledged by this Grant
    * @param manager_xact_id manager's transaction id
    */
  def makeGrant(rel: ReleaseFromSrc, manager_xact_id: UInt): GrantToDst = {
    Wire(Grant(
      dst = rel.client_id,
      is_builtin_type = true.B,
      g_type = Grant.voluntaryAckType,
      client_xact_id = rel.client_xact_id,
      manager_xact_id = manager_xact_id))
  }

  /** Construct an appropriate [[uncore.GrantToDst]] to respond to an [[uncore.Acquire]]
    *
    * May contain single or multiple beats of data, or just be a permissions upgrade.
    *
    * @param acq Acquire message being responded to by this Grant
    * @param manager_xact_id manager's transaction id
    * @param addr_beat beat id of the data
    * @param data data being refilled to the original requestor
    */
  def makeGrant(
      acq: AcquireFromSrc,
      manager_xact_id: UInt, 
      addr_beat: UInt = 0.U,
      data: UInt = 0.U): GrantToDst = {
    Wire(Grant(
      dst = acq.client_id,
      is_builtin_type = acq.isBuiltInType(),
      g_type = Mux(acq.isBuiltInType(), 
                     acq.getBuiltInGrantType(),
                     co.getGrantType(acq, this)),
      client_xact_id = acq.client_xact_id,
      manager_xact_id = manager_xact_id,
      addr_beat = addr_beat,
      data = data))
  }

  /** Construct an [[uncore.GrantToDst]] to respond to an [[uncore.Acquire]] with some overrides
    *
    * Used to respond to secondary misses merged into this transaction.
    * May contain single or multiple beats of data.
    *
    * @param pri Primary miss's Acquire message, used to get g_type and dst
    * @param sec Secondary miss info, used to get beat and client_xact_id
    * @param manager_xact_id manager's transaction id
    * @param data data being refilled to the original requestor
    */
//  def makeGrant(
//      pri: AcquireFromSrc,
//      sec: SecondaryMissInfo,
//      manager_xact_id: UInt,
//      data: UInt): GrantToDst = {
//    val g = makeGrant(pri, manager_xact_id, sec.addr_beat, data)
//    g.client_xact_id := sec.client_xact_id
//    g
//  }
    
  /** New metadata after receiving a [[uncore.ReleaseFromSrc]]
    *
    * @param incoming the incoming [[uncore.ReleaseFromSrc]]
    */
  def onRelease(incoming: ReleaseFromSrc): ManagerMetadata =
    Wire(co.managerMetadataOnRelease(incoming, incoming.client_id, this))

  /** New metadata after sending a [[uncore.GrantToDst]]
    *
    * @param outgoing the outgoing [[uncore.GrantToDst]]
    */
  def onGrant(outgoing: GrantToDst): ManagerMetadata =
    Wire(co.managerMetadataOnGrant(outgoing, outgoing.client_id, this))
}

/** Factories for ManagerMetadata, including on reset */
object ManagerMetadata {
  def apply(sharers: UInt, state: UInt = UInt(width = 0.W))(implicit p: Parameters) = {
    val meta = new ManagerMetadata
    //meta.state := state TODO: Fix 0-width wires in Chisel 
    meta.sharers := sharers
    meta
  }
  def apply()(implicit p: Parameters) = {
    val meta = new ManagerMetadata
    //meta.state := UInt(width = 0) TODO: Fix 0-width wires in Chisel 
    meta.sharers := meta.co.dir.flush
    meta
  }
  def onReset(implicit p: Parameters) = new ManagerMetadata().co.managerMetadataOnReset
}

/** HierarchicalMetadata is used in a cache in a multi-level memory hierarchy
  * that is a manager with respect to some inner caches and a client with
  * respect to some outer cache.
  *
  * This class makes use of two different sets of TileLink parameters, which are
  * applied by contextually mapping [[uncore.TLId]] to one of 
  * [[uncore.InnerTLId]] or [[uncore.OuterTLId]].
  */ 
//class HierarchicalMetadata extends CoherenceMetadata {
//  val inner: ManagerMetadata = Wire(new ManagerMetadata, {case TLId => params(InnerTLId)})
//  val outer: ClientMetadata = Wire(new ClientMetadata, {case TLId => params(OuterTLId)})
//  def ===(rhs: HierarchicalMetadata): Bool =
//    this.inner === rhs.inner && this.outer === rhs.outer
//  def !=(rhs: HierarchicalMetadata): Bool = !this.===(rhs)
//}
//
///** Factories for HierarchicalMetadata, including on reset */
//object HierarchicalMetadata {
//  def apply(inner: ManagerMetadata, outer: ClientMetadata): HierarchicalMetadata = {
//    val m = new HierarchicalMetadata
//    m.inner := inner
//    m.outer := outer
//    m
//  }
//  def onReset: HierarchicalMetadata = apply(ManagerMetadata.onReset, ClientMetadata.onReset)
//}

/** Identifies the TLId of the inner network in a hierarchical cache controller */ 
case object InnerTLId extends Field[String]
/** Identifies the TLId of the outer network in a hierarchical cache controller */ 
case object OuterTLId extends Field[String]
