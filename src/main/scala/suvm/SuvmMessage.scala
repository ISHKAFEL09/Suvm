package suvm

import SuvmObjectGlobals._
import SuvmImplicits._

class SuvmMessage {

}

object SuvmMessage {
  def suvmInfo(id: String,
               msg: String,
               verbosity: Int,
               reportEnabled: (Int, SuvmSeverity.Value, String) => Boolean,
               reportInfo: (String, String, Int, String, Int, String, Boolean) => Unit
              ): Unit = {
    if (reportEnabled(verbosity, SuvmSeverity.UVM_INFO, id)) {
      reportInfo(id, msg, verbosity, "", 0, "", true)
    }
  }

  def suvmWarning(id: String,
               msg: String,
               reportEnabled: (Int, SuvmSeverity.Value, String) => Boolean,
               reportWarn: (String, String, Int, String, Int, String, Boolean) => Unit
              ): Unit = {
    if (reportEnabled(SuvmVerbosity.UVM_NONE, SuvmSeverity.UVM_WARNING, id)) {
      reportWarn(id, msg, SuvmVerbosity.UVM_NONE, "", 0, "", true)
    }
  }

  def suvmError(id: String,
                  msg: String,
                  reportEnabled: (Int, SuvmSeverity.Value, String) => Boolean,
                  reportError: (String, String, Int, String, Int, String, Boolean) => Unit
                 ): Unit = {
    if (reportEnabled(SuvmVerbosity.UVM_NONE, SuvmSeverity.UVM_WARNING, id)) {
      reportError(id, msg, SuvmVerbosity.UVM_NONE, "", 0, "", true)
    }
  }
}
