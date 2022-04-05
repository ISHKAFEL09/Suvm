package suvm

abstract class SuvmLinkBase extends SuvmObject {

}

class SuvmParentChildLink(val name: String) extends SuvmLinkBase {

}

object SuvmParentChildLink {
  def getLink(lhs: SuvmObject, rhs: SuvmObject, name: String = "PcLink"): SuvmParentChildLink =
    ???
}