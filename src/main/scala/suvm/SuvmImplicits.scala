package suvm

import java.io.File
import scala.language.implicitConversions

case class SuvmConfig(SUVM_ENABLE_DEPRECATED_API: Boolean)

object SuvmImplicits {
  private def getConfigFromFile(cfg: String): SuvmConfig = ???

  implicit val suvmDefaultConfig: SuvmConfig = getConfigFromFile("suvm.conf")

  implicit def Int2Time(x: Int): SuvmTime = new SuvmTime(x.toDouble)

  implicit def Double2Time(x: Double): SuvmTime = new SuvmTime(x)
}
