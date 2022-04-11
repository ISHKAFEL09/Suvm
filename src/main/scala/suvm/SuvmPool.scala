package suvm

abstract class SuvmPool[KEY, T](val name: String = "") extends SuvmObject {
  def defaultValue(key: KEY): T

  def first: Option[KEY] = pool.keys.headOption

  def tail: Iterable[KEY] = pool.keys.tail

  def num: Int = pool.size

  def foreach[U](f: ((KEY, T)) => U): Unit = pool.foreach(f)

  def isEmpty: Boolean = pool.isEmpty

  def nonEmpty: Boolean = pool.nonEmpty

  def get(key: KEY): T = pool.getOrElseUpdate(key, defaultValue(key))

  def exists(key: KEY): Boolean = pool.contains(key)

  def add(key: KEY, item: T): Unit = pool.update(key, item)

  def keys: Iterable[KEY] = pool.keys

  def values: Iterable[T] = pool.values

  protected val pool = scala.collection.mutable.HashMap.empty[KEY, T]
}

abstract class SuvmObjectStringPool[T <: SuvmObject](name: String) extends SuvmPool[String, T](name)

class SuvmEventPool(name: String) extends SuvmObjectStringPool[SuvmEvent[SuvmObject]](name) {
  override def defaultValue(key: String): SuvmEvent[SuvmObject] = new SuvmEvent[SuvmObject](key)
}