package rockets.tilelink

import rockets.core.consts.MemOps._
import rockets.params.config.Parameters
import spinal.core._

trait HasCoherenceMetaData extends TLBundle {
  val co = tlCoh
}

case class ClientMetaData()(implicit p: Parameters)
    extends HasCoherenceMetaData {
  val state = UInt(widthOf(co.clientEnum()) bits)

  /** make a Release based on this meta and [[Probe]] */
  def makeRelease(
      probe: Probe,
      beatAddr: UInt = U(0),
      data: UInt = U(0)
  ): Release = {
    Release(
      voluntary = False,
      typ = co.getReleaseType(probe, this),
      clientXactId = U(0),
      blockAddr = probe.blockAddr,
      beatAddr = beatAddr,
      data = data
    )
  }

  /** new meta after receiving a [[Probe]] */
  def onProbe(probe: Probe): ClientMetaData =
    co.clientMetaDataOnProbe(probe, this)

  def requiresVoluntaryWriteback(): Bool =
    co.requiresReleaseOnCacheControl(M_FLUSH, this)

  override def clone(): Bundle = ClientMetaData()
}

object ClientMetaData {
  def apply(state: UInt)(implicit p: Parameters): ClientMetaData = {
    val meta = ClientMetaData()
    meta.state := state
    meta
  }
}
