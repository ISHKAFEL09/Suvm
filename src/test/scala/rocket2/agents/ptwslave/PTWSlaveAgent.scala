package rocket2.agents.ptwslave

import uvm._

class PTWSlaveAgent(name: String, parent: UVMComponent, cfg: PTWSlaveAgentConfig)
  extends UVMComponent(name, Some(parent)) {
  var driver: Option[PTWSlaveDriver] = None

  override def buildPhase(phase: UVMPhase): Unit = {
    driver = Some(create("ptwSlaveDriver", this) { case (s, p) =>
      new PTWSlaveDriver(s, p, cfg.bus)
    })
  }
}
