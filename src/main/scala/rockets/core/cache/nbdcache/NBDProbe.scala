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
    val sMetaRead, sMetaResp = State()

    /** check the mshr ready in s2 */
    val sCheckMSHR = State()

    /** send release to tl if no need to write back */
    val sRelease = State()

    /** write back victim entry */
    val sWbReq, sWbResp = State()

    /** update meta data */
    val sMetaWrite = State()

    /** begin to read the meta data when got new probe request */
    sIdle.whenIsActive {
      when(io.probe.valid) {
        goto(sMetaRead)
      }
    }

    /** meta read fire, goto s1 */
    sMetaRead.whenIsActive {
      when(io.metaRead.ready) {
        goto(sMetaResp)
      }
    }

    /** goto s2 */
    sMetaResp.whenIsActive {
      goto(sCheckMSHR)
    }

    /** check whether mshr has hazard in s2 if enter the same entry:
      * 1. mshr in state before refill and could write data to the victim, which probe may release to tl
      * 2. mshr has just write meta data, which cause probe got the wrong state
      */

  }

  val reqReg = RegNextWhen(io.probe.payload, fsm.isExiting(fsm.sIdle))
}
