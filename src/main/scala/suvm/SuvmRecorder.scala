package suvm

abstract class SuvmRecorder extends SuvmPolicy {
  def getHandle: Int

  def close(closeTime: Time = 0): Unit

  def free(closeTime: Time = 0): Unit
}

object SuvmRecorder {
  def getRecorderFromHandle(id: Int): Option[SuvmRecorder] = ???
}