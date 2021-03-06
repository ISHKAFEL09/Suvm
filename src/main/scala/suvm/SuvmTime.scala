package suvm

sealed trait SuvmTimeUnit {
  val unit: Double
  val name: String
}

object nsUnit extends SuvmTimeUnit {
  override val unit: Double = 1
  override val name: String = "ns"
}

object msUnit extends SuvmTimeUnit {
  override val unit: Double = 1000 * 1000
  override val name: String = "ms"
}

object sUnit extends SuvmTimeUnit {
  override val unit: Double = 1000 * 1000 * 1000
  override val name: String = "s"
}

trait Time {
  val value: Double
  val unit: SuvmTimeUnit

  override def toString: String = s"$value${unit.name}"
}

class SuvmTime(x: Double) {
  def ms: Time = new Time {
    override val value: Double = x
    override val unit: SuvmTimeUnit = msUnit
  }

  def s: Time = new Time {
    override val value: Double = x
    override val unit: SuvmTimeUnit = sUnit
  }
}
