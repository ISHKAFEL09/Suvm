package suvm

class SuvmComparer(val name: String) extends SuvmPolicy {
  def compareObject(name: String, lsh: SuvmObject, rhs: SuvmObject): Boolean =
    ???
}

object SuvmComparer {
  def getDefault: SuvmComparer = SuvmCoreService.getDefaultComparer
}