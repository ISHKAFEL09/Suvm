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
      domain.add(UVMCommonPhase("build"))
      domain.add(UVMCommonPhase("connect"))
      domain.add(UVMCommonPhase("run"))
      domain.add(UVMCommonPhase("extract"))
      domain.add(UVMCommonPhase("check"))
      domain.add(UVMCommonPhase("report"))
      domain.add(UVMCommonPhase("final"))
      domain
    })
  }
}
