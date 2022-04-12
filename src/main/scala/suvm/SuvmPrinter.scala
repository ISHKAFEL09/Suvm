package suvm

import java.io.File
import SuvmObjectGlobals._

abstract class SuvmPrinter extends SuvmPolicy {
  private class PrinterKnobs {
    var mcd: Option[File] = None
    var showRoot: Boolean = false
  }
  private val knobs: PrinterKnobs = new PrinterKnobs

  def getFile: Option[File] = knobs.mcd

  def getRootEnabled: Boolean = knobs.showRoot

  def printArrayHeader(name: String, size: Int, arrayType: String = "array", scopeSeparator: Char = '.'): Unit =
    println(s"$name: $size")

  def printArrayFooter(size: Int = 0): Unit = println("footer")

  def printObject(name: String, value: SuvmObject, scopeSeparator: Char = '.'): Unit =
    println(s"$name: $value")

  def emit: String = ""

  def printTime(name: String, value: Time, scopeSeparator: Char = '.'): Unit = {
    println(s"$name: ${value.value} ${value.unit.name}")
  }

  def printGeneric(name: String, typeName: String, size: Int, value: String, scopeSeparator: Char = '.'): Unit = {
    println(s"$name: $value")
  }

  def printString(name: String, value: String, scopeSeparator: Char = '.'): Unit = {
    println(s"$name: $value")
  }

  def printField(name: String, value: SuvmBitstream, size: Int,
                 radix: SuvmRadix.Value = SuvmRadix.UVM_NORADIX,
                 scopeSeparator: Char = '.', typeName: String = ""): Unit = {
    println(f"$name: $value%x")
  }

  def getLinePrefix: String = prefix

  def setLinePrefix(p: String): Unit = prefix = p

  private var prefix: String = ""
}

class SuvmTablePrinter(val name: String) extends SuvmPrinter {

}

object SuvmPrinter {
  def getDefault: SuvmPrinter = SuvmCoreService.getDefaultPrinter
}

object SuvmTablePrinter {
  private var mDefaultTablePrinter: SuvmTablePrinter = new SuvmTablePrinter("SuvmDefaultTablePrinter")
  def getDefault: SuvmTablePrinter = mDefaultTablePrinter
}