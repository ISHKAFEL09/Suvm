package suvm

abstract class SuvmComponent(name: String) extends SuvmReportObject(name) {
  def mDoPreAbort(): Unit = ???
}
