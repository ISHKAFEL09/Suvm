package uvm

import ENUM_PHASE_TYPE._
import ENUM_PHASE_STATE._

class UVMDomain(name: String) extends UVMPhase(name, UVM_PHASE_DOMAIN) {
  UVMDomain.mDomains(name) = this
}

object UVMDomain {
  private val mDomains = collection.mutable.HashMap.empty[String, UVMDomain]

  def getCommonDomain: UVMDomain = {
    mDomains.getOrElseUpdate("common", {
      val domain = new UVMDomain("common")
      domain.add()
    })
  }
}
