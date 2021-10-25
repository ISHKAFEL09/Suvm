package suvm

import java.io.File

abstract class SuvmPrinter(name: String) extends SuvmPolicy(name) {
  private class PrinterKnobs {
    var mcd: Option[File] = None
    var showRoot: Boolean = false
  }
  private val knobs: PrinterKnobs = new PrinterKnobs

  def getFile: Option[File] = knobs.mcd

  def getRootEnabled: Boolean = knobs.showRoot

  def printObject(name: String, value: SuvmObject, scopeSeparator: Char = '.'): Unit

  def emit: String = ""
}

class SuvmTablePrinter(name: String) extends SuvmPrinter(name) {

}

object SuvmPrinter {
  def getDefault: SuvmPrinter = SuvmCoreService.getDefaultPrinter
}

object SuvmTablePrinter {
  private var mDefaultTablePrinter: SuvmTablePrinter = new SuvmTablePrinter("SuvmDefaultTablePrinter")
  def getDefault: SuvmTablePrinter = mDefaultTablePrinter
}