package suvm

class SuvmPacker(name: String) extends SuvmPolicy(name) {
  def packObject(value: SuvmObject): SuvmPacker
  def unPackObject(value: SuvmObject): Unit

  def getPacked[T]: Seq[T]
  def setPacked[T](stream: Seq[T]): Seq[T]
  def getPackedSize: Int

}

object SuvmPacker {
  def getDefault: SuvmPacker = SuvmCoreService.getDefaultPacker
}