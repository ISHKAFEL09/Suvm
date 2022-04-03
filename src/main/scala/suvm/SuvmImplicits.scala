package suvm

import java.io.File

case class SuvmConfig(SUVM_ENABLE_DEPRECATED_API: Boolean)

object SuvmImplicits {
  private def getConfigFromFile(cfg: String): SuvmConfig = ???

  implicit val suvmDefaultConfig: SuvmConfig = getConfigFromFile("suvm.conf")
}
