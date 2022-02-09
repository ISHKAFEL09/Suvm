package suvm

abstract class SuvmPool[KEY, T] extends SuvmObject {
  val pool = scala.collection.mutable.HashMap.empty[KEY, T]
}

abstract class SuvmObjectStringPool[T <: SuvmObject] extends SuvmPool[String, T] {
  def apply(key: String): T
  def get(key: String): T = pool.getOrElseUpdate(key, apply(key))
}

class SuvmEventPool extends SuvmObjectStringPool[SuvmEvent[SuvmObject]] {
  override def apply(key: String): SuvmEvent[SuvmObject] = new SuvmEvent[SuvmObject](key)
}