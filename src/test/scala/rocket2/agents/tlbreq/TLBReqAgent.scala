package rocket2.agents.tlbreq

import uvm._

class TLBReqSequencer(name: String, parent: UVMComponent)
  extends UVMSequencer[TLBReqItem, TLBReqItem](name, Some(parent))

class TLBReqAgent(name: String, parent: UVMComponent, cfg: TLBReqAgentConfig)
  extends UVMComponent(name, Some(parent)) {

  var driver: Option[TLBReqDriver] = None
  var monitor: Option[TLBReqMonitor] = None
  var sqr: Option[TLBReqSequencer] = None

  override def buildPhase(phase: UVMPhase): Unit = {
    driver = Some(create("driver", this) { case (s, p) =>
      new TLBReqDriver(s, p, cfg.driverIF)
    })

    monitor = Some(create("monitor", this) { case (s, p) =>
      new TLBReqMonitor(s, p, cfg.monitorIF)
    })

    sqr = Some(create("sqr", this) { case (s, p) =>
      new TLBReqSequencer(s, p)
    })
  }

  override def connectPhase(phase: UVMPhase): Unit = {
    driver.get.seqItemPort.connect(sqr.get.seqItemExport)
  }
}
