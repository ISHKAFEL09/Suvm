package suvm

abstract class SuvmEventBase extends SuvmObject

abstract class SuvmEvent extends SuvmEventBase {
  type T <: SuvmObject
  def trigger(data: T = getDefaultData): Unit

  def getDefaultData: T
}
