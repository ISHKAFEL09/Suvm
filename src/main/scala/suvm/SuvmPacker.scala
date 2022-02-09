package suvm

class SuvmPacker(name: String) extends SuvmPolicy(name) {
  def packObject(value: SuvmObject): SuvmPacker
  def unPackObject(value: SuvmObject): Unit

  def getPackedBool: Seq[Boolean]
  def getPackedInt: Seq[Int]
  def getPackedByte: Seq[Byte]
  def getPackedLong: Seq[Long]
  def setPacked[T](stream: Seq[T]): Seq[T]
  def getPackedSize: Int

}

object SuvmPacker {
  def getDefault: SuvmPacker = SuvmCoreService.getDefaultPacker
}