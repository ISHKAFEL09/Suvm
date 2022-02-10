package suvm

abstract class SuvmPool extends SuvmObject {
  type KEY <: AnyVal
  type T <: SuvmVoid
  val pool = scala.collection.mutable.HashMap.empty[KEY, T]
}

abstract class SuvmObjectStringPool extends SuvmPool {
  type KEY = String
  def apply(key: String): T
  def get(key: String): T = pool.getOrElseUpdate(key, apply(key))
}

class SuvmEventPool extends SuvmObjectStringPool {
  type T = SuvmEvent { type T = SuvmObject}
  override def apply(key: String): T = new T(key)
}