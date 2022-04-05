package suvm

abstract class SuvmRecorder extends SuvmPolicy {
  import SuvmImplicits._

  def getHandle: Int

  def close(closeTime: Time = 0.s): Unit

  def free(closeTime: Time = 0.s): Unit
}

object SuvmRecorder {
  def getRecorderFromHandle(id: Int): Option[SuvmRecorder] = ???
}