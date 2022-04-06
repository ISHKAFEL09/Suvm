package suvm

class SuvmObjectRegistry[+T](fac: String => T, Tname: String = "<unknown>") extends SuvmObjectWrapper {
  def createObject(name: String): T = fac(name)

  def create(name: String = "",
             parent: Option[SuvmComponent] = None,
             contxt: String = ""): T = {
    fac(name)
  } // TODO:
}

object SuvmObjectRegistry{
}

class SuvmRegistry {

}
