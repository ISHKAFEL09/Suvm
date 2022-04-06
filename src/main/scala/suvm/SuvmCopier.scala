package suvm

class SuvmCopier(val name: String) extends SuvmPolicy {
  def copyObject(lhs: SuvmObject, rhs: SuvmObject): Option[SuvmObject] = ???

}
