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

  val hit = Bool()
  val dirty = Bool()

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

    /** check whether mshr has hazard in s2 if enter the same entry and:
      * 1. mshr in state before refill and would release victim and clear the meta,
      * which probe should avoid releasing it again.
      * 2. or mshr has just update meta, so probe need to make sure has read the correct meta,
      * means mshr has not write the same meta in last two cycles.
      *
      * if no hazard, goto release to writeback the victim. otherwise, read the meta again.
      * if probe is busy, later incoming request to same entry must be nacked.
      */
    sCheckMSHR.whenIsActive {
      when(io.mshrReady) {
        goto(sRelease)
      }.otherwise {
        goto(sMetaRead)
      }
    }

    /** 1. if entry hit and dirty, writeback
      * 2. if entry hit and clean, update meta
      * 3. otherwise release and finish
      */
    sRelease.whenIsActive {
      when(io.release.ready) {
        when(hit) {
          when(dirty) {
            goto(sWbReq)
          }.otherwise {
            goto(sMetaWrite)
          }
        }.otherwise {
          goto(sIdle)
        }
      }
    }

    /** writeback, wait handshake */
    sWbReq.whenIsActive {
      when(io.wbReq.ready) {
        goto(sWbResp)
      }
    }

    /** wb req accepted, but still need to wait wb idle to clear meta
      * maybe to nack successor cpu requests
      */
    sWbResp.whenIsActive {
      when(io.wbReq.ready) {
        goto(sMetaWrite)
      }
    }

    /** release done, clear the meta */
    sMetaWrite.whenIsActive {
      when(io.metaWrite.ready) {
        goto(sIdle)
      }
    }
  }

  val reqReg = RegNextWhen(io.probe.payload, fsm.isExiting(fsm.sIdle))

  /** meta data is valid in s2 */
  val cohReg = RegNextWhen(io.metaData, fsm.isExiting(fsm.sCheckMSHR))
  val wayReg = RegNextWhen(io.way, fsm.isExiting(fsm.sCheckMSHR))
  hit := wayReg.orR
  dirty := cohReg.requiresVoluntaryWriteback()
  val release = cohReg.makeRelease(reqReg)

  /** req */
  io.probe.ready := fsm.isActive(fsm.sIdle)

  /** meta request */
  io.metaRead.valid := fsm.isActive(fsm.sMetaRead)
  io.metaRead.payload.idx := reqReg.blockAddr(0, idxBits bits)
  io.metaRead.payload.tag := reqReg.blockAddr(idxBits, tagBits bits)
  io.metaWrite.valid := fsm.isActive(fsm.sMetaWrite)
  io.metaWrite.payload.way := wayReg
  io.metaWrite.payload.idx := reqReg.blockAddr(0, idxBits bits)
  io.metaWrite.payload.data.tag := reqReg.blockAddr(idxBits, tagBits bits)
  io.metaWrite.payload.data.coh := cohReg.onProbe(reqReg)

  /** wb request */
  io.wbReq.valid := fsm.isActive(fsm.sWbReq)
  io.wbReq.payload.assignSomeByName(release)
  io.wbReq.payload.way := wayReg

  /** release */
  io.release.valid := fsm.isActive(fsm.sRelease) && !(hit && dirty)
  io.release.payload := release
}

object NBDProbe extends App {
  import rockets._
  import rockets.tile.Configs.SimpleConfig

  generate(NBDProbe())
}
