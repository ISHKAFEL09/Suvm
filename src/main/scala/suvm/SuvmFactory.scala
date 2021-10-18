package suvm

abstract class SuvmObjectWrapper

abstract class SuvmFactory {

}

object SuvmFactory {
  private val mTypeNames = scala.collection.mutable.HashMap.empty[String, SuvmObjectWrapper]

  def findWrapperByName(typeName: String): Option[SuvmObjectWrapper] = {
    mTypeNames.get(typeName)
  }
}
