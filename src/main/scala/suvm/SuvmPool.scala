package suvm

abstract class SuvmPool extends SuvmObject {
  type KEY
  type T <: SuvmVoid
  val pool = scala.collection.mutable.HashMap.empty[KEY, T]
}

abstract class SuvmObjectStringPool extends SuvmPool {
  type KEY = String
  def apply(key: String): T
  def get(key: String): T = pool.getOrElseUpdate(key, apply(key))
}

class SuvmEventPool(val name: String) extends SuvmObjectStringPool {
  type T = SuvmEvent { type T = SuvmObject}
  override def apply(key: String): T = new SuvmEvent {
    override type T = SuvmObject
    override val name: String = key
  }
}