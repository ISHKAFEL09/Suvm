package rockets

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rockets.core.front.{BTB, BtbIO}
import spinal.core._
import spinal.core.sim._
import uvm._

import scala.util.Random

case class BTBIF(io: BtbIO, domain: ClockDomain)

case class BTBItem(name: String,
                   hit: Boolean,
                   wen: Boolean,
                   currentPC: BigInt,
                   target: BigInt,
                   correctPC: BigInt,
                   correctTarget: BigInt) extends UVMSequenceItem(name)

case class BTBDriver(name: String, parent: UVMComponent, bus: BTBIF)
  extends UVMDriver[BTBItem, BTBItem](name, Some(parent)) {
  override def runPhase(phase: UVMPhase): Unit = {
    while (true) {
      val item = seqItemPort.getNextItem
      bus.io.currentPC #= item.currentPC
      bus.io.target #= item.target
      bus.io.correctPC #= item.correctPC
      bus.io.correctTarget #= item.correctTarget
      bus.domain.waitSampling()
      val rsp = item.copy(hit = bus.io.hit.toBoolean, wen = bus.io.wen.toBoolean)
      println(s"rsp: $rsp")
      seqItemPort.itemDone()
    }
  }
}

class BTBSequencer(name: String, parent: UVMComponent) extends UVMSequencer[BTBItem, BTBItem](name, Some(parent))

case class BTBAgent(name: String, parent: UVMComponent, bus: BTBIF) extends UVMAgent(name, Some(parent)) {
  var driver: Option[BTBDriver] = None
  var sqr: Option[BTBSequencer] = None

  override def buildPhase(phase: UVMPhase): Unit = {
    driver = Some(create("driver", this) {case(s, p) => BTBDriver(s, p, bus)})
    sqr = Some(create("sqr", this) {case(s, p) => new BTBSequencer(s, p)})
  }

  override def connectPhase(phase: UVMPhase): Unit =
    driver.get.seqItemPort.connect(sqr.get.seqItemExport)
}

class Harness extends HarnessBase {
  val dut = BTB()
  dut.io.simPublic()

  val btbIF = BTBIF(dut.io, dut.clockDomain)

  override def init(): Unit = {
    dut.clockDomain.forkStimulus(1000)
  }
}

case class BTBSmokeSeq(name: String) extends UVMSequenceBase(name) {
  override def body(): Unit = {
    for (i <- 0 to 100) {
      val item = BTBItem(
        "item",
        Random.nextBoolean(),
        Random.nextBoolean(),
        i,
        Random.between(0, 100000),
        Random.between(0, 100000),
        Random.between(0, 100000)
      )
      startItem(item)
      finishItem(item)
    }
  }
}

case class BTBTestBase(name: String, bus: BTBIF) extends UVMTest(name, None) {
  var btbAgent: Option[BTBAgent] = None

  override def buildPhase(phase: UVMPhase): Unit = {
    btbAgent = Some(create("btbAgent", this) {case(s, p) => new BTBAgent(s, p, bus)})
  }

  override def runPhase(phase: UVMPhase): Unit = {
    phase.raiseObjection(phase)
    BTBSmokeSeq("seq").start(btbAgent.get.sqr.get)
    phase.dropObjection(phase)
  }
}

class BTBSmokeTest extends AnyFlatSpec with Matchers {
  behavior of "BTBSmokeTest"

//  val simulator: (Harness => BTBTestBase) => Unit = uvmRun(new Harness)

  it should "pass btb smoke test" in {
    uvmRun(new Harness) { (h: Harness) =>
      BTBTestBase("test", h.btbIF)
    }
  }
}
