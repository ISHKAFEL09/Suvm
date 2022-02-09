package suvm

abstract class SuvmEventBase(name: String) extends SuvmObject(name)

class SuvmEvent[T](name: String) extends SuvmEventBase(name) {
  def trigger(data: T): Unit
}
