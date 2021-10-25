package suvm

class SuvmComparer(name: String) extends SuvmPolicy(name) {
  def compareObject(name: String, lsh: SuvmObject, rhs: SuvmObject): Boolean
}

object SuvmComparer {
  def getDefault: SuvmComparer = SuvmCoreService.getDefaultComparer
}