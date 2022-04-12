package suvm

abstract class SuvmPolicy extends SuvmObject {
  def getActiveObjectDepth: Boolean = false

  def flush(): Unit = {}
}
