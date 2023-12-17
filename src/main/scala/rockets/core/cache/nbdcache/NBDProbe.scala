package rockets.core.cache.nbdcache

import rockets.params.config._
import rockets.tilelink._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

/** probe with MSHR index */
class ProbeWithXactId(implicit p: Parameters)
    extends Probe
    with HasTLClientXactId

case class NBDProbeIO()(implicit p: Parameters)
    extends NBDBundle
    with IMasterSlave {
  val probe = Stream(new ProbeWithXactId())
  val metaRead = Stream(NBDMetaReadReq())
  val metaData = ClientMetaData()
  val way = UInt(nWays bits)
  val wbReq = Stream(NBDWriteBackReq())
  val mshrReady = Bool()
  val metaWrite = Stream(NBDMetaWriteReq())
  val release = Stream(new Release())

  override def asMaster(): Unit = {
    master(probe)
    slave(release, metaRead, metaWrite, wbReq)
    out(metaData, way, mshrReady)
  }
}

case class NBDProbe()(implicit p: Parameters) extends NBDComponent {
  val io = slave(NBDProbeIO())

  /** probe fsm */
  val fsm = new StateMachine {

    /** entry state */
    val sIdle = StateEntryPoint()

    /** request to read meta */
    val sMetaRead = State()
  }
}
