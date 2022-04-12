import java.io.{File, FileWriter}
import scala.util.Random

package object suvm {
  val SUVM_REVERSION = "SUVM:1.0"

  case class SuvmConfig(SUVM_ENABLE_DEPRECATED_API: Boolean)

  import SuvmImplicits._

  private[suvm] var realtime: Time = 0.ms

  val SuvmDefaultTimeout: Time = 9200.s
  val SuvmGlobalRandomSeed = new Random()
  SuvmGlobalRandomSeed.setSeed(0)

  private[suvm] class SuvmSeedMap {
    val seedTable = scala.collection.mutable.HashMap.empty[String, Int]
    val count = scala.collection.mutable.HashMap.empty[String, Int]
  }

  private[suvm] val SuvmRandomSeedTableLookup = scala.collection.mutable.HashMap.empty[String, SuvmSeedMap]

  def fWrite(file: Option[File], msg: String, append: Boolean = true): Unit = {
    if (file.isEmpty) { println(msg) } else {
      val writer = new FileWriter(file.get, append)
      try {
        writer.write(msg)
      }
      finally {
        writer.close()
      }
    }
  }

  def suvmIsMatch(p: String, i: String): Boolean = {
    ("(?i)" + p.replaceFirst("^\\+", "\\\\+")).r matches i
  }

  def runTest(testName: String = "")(implicit config: SuvmConfig): Unit = {
    SuvmRoot.init.runTest(testName)
  }

  lazy val SuvmTop: SuvmRoot = SuvmRoot.get

  type SuvmBitstream = BigInt
}