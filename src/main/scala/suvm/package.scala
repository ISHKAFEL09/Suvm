import dev.SuvmFork

import scala.util.Random

package object suvm {
  object SuvmFieldFlag extends Enumeration {
    val UVM_BIN = Value
    val UVM_DEC = Value
    val UVM_UNSIGNED = Value
    val UVM_UNFORMAT2 = Value
    val UVM_UNFORMAT4 = Value
    val UVM_OCT = Value
    val UVM_HEX = Value
    val UVM_STRING = Value
    val UVM_TIME = Value
    val UVM_ENUM = Value
    val UVM_REAL = Value
    val UVM_REAL_DEC = Value
    val UVM_REAL_EXP = Value
    val UVM_NORADIX = Value
  }

  object SuvmVerbosity extends Enumeration {
    val UVM_NONE = Value(0)
    val UVM_LOW = Value(100)
    val UVM_MEDIUM = Value(200)
    val UVM_HIGH = Value(300)
    val UVM_FULL = Value(400)
    val UVM_DEBUG = Value(500)
  }

  val SuvmGlobalRandomSeed = new Random()
  SuvmGlobalRandomSeed.setSeed(0)

  private[suvm] class SuvmSeedMap {
    val seedTable = scala.collection.mutable.HashMap.empty[String, Int]
    val count = scala.collection.mutable.HashMap.empty[String, Int]
  }

  private[suvm] val SuvmRandomSeedTableLookup = scala.collection.mutable.HashMap.empty[String, SuvmSeedMap]

  def SuvmReportError(id: String, msg: String, verbosity: SuvmVerbosity.Value = SuvmVerbosity.UVM_NONE,
                      fileName: String = "", line: Int = 0, contextName: String = "",
                      reportEnabledChecked: Boolean = false): Unit = {
    // TODO
  }
}