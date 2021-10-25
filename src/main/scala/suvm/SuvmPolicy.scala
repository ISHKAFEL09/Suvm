package suvm

abstract class SuvmPolicy(name: String) extends SuvmObject(name) {
  def getActiveObjectDepth: Boolean
  def flush(): Unit
}
