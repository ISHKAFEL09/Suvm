package suvm

abstract class SuvmEventBase(name: String) extends SuvmObject(name)

abstract class SuvmEvent(name: String) extends SuvmEventBase(name) {
  type T <: SuvmObject
  def trigger(data: T = getDefaultData): Unit

  def getDefaultData: T
}
