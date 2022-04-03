package suvm

class SuvmPacker(val name: String) extends SuvmPolicy {
  def packObject(value: SuvmObject): SuvmPacker =
    ???

  def unPackObject(value: SuvmObject): Unit =
    ???

  def getPackedBool: Seq[Boolean] =
    ???

  def getPackedInt: Seq[Int] =
    ???

  def getPackedByte: Seq[Byte] =
    ???

  def getPackedLong: Seq[Long] =
    ???

  def setPacked[T](stream: Seq[T])(implicit f: T => T): Seq[T] = stream.map(f)

  def getPackedSize: Int =
    ???

  override def getActiveObjectDepth: Boolean = ???

  override def flush(): Unit = ???
}

object SuvmPacker {
  def getDefault: SuvmPacker = SuvmCoreService.getDefaultPacker
}