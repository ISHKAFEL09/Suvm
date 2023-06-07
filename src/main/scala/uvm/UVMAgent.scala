package uvm

abstract class UVMAgent(name: String, parent: Option[UVMComponent]) extends UVMComponent(name, parent) {
  val isActive: ENUM_ACTIVE_PASSIVE.Value = ENUM_ACTIVE_PASSIVE.UVM_Active

  def getIsActive: uvm.ENUM_ACTIVE_PASSIVE.Value = isActive
}
