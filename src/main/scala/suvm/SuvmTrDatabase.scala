package suvm

class SuvmTrDatabase(val name: String) extends SuvmObject {
  def establishLink(link: SuvmLinkBase): Unit = ???

  def openStream(name: String, scope: String = "", typeName: String = ""): Option[SuvmTrStream] = ???
}
