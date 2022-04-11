package suvm

abstract class SuvmEventBase extends SuvmObject

class SuvmEvent[T <: SuvmObject](val name: String) extends SuvmEventBase {
  def trigger(data: T = getDefaultData): Unit =
    ???

  def getDefaultData: T =
    ???
}
