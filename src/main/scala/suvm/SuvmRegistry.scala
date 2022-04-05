package suvm

class SuvmObjectRegistry[+T](fac: String => T, Tname: String = "<unknown>") extends SuvmObjectWrapper {
  def createObject(name: String): T = fac(name)
}

object SuvmObjectRegistry{
}

class SuvmRegistry {

}
