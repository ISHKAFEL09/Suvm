package suvm

abstract class SuvmTrDatabase extends SuvmObject {
  def establishLink(link: SuvmLinkBase): Unit
}
