package suvm

abstract class SuvmPolicy extends SuvmObject {
  def getActiveObjectDepth: Boolean
  def flush(): Unit
}
