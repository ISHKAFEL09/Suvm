package uvm

case class UVMEvent(name: String = "UVMEvent") {
  private var mTrig = false

  def trigger(): Unit = {
    mTrig = true
    debugLog(s"trigger event $name")
    ~>(1)
    mTrig = false
  }

  def isTriggered: Boolean = mTrig
}
