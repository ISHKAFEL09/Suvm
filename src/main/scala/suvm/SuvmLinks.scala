package suvm

abstract class SuvmLinkBase(name: String) extends SuvmObject(name) {

}

class SuvmParentChildLink(name: String) extends SuvmLinkBase(name) {

}

object SuvmParentChildLink {
  def getLink(lhs: SuvmObject, rhs: SuvmObject, name: String = "PcLink"): SuvmParentChildLink
}