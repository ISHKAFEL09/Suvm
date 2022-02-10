package suvm

abstract class SuvmRecorder extends SuvmPolicy {
  def getHandle: Int
}

object SuvmRecorder {
  def getRecorderFromHandle(id: Int): Option[SuvmRecorder] = ???
}