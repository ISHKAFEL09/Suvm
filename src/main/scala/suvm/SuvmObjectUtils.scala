package suvm

trait SuvmObjectUtils[T] {
  def create: String => T

  object typeId extends SuvmObjectRegistry(create, create("").getClass.getSimpleName)
}
