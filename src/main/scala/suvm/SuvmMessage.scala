package suvm

import SuvmObjectGlobals._

class SuvmMessage {

}

object SuvmMessage {
  def suvmInfo(id: String,
               msg: String,
               verbosity: SuvmVerbosity.Value,
               reportEnabled: (SuvmVerbosity.Value, SuvmSeverity.Value, String) => Boolean,
               reportInfo: (String, String, SuvmVerbosity.Value, String, Int, String, Boolean) => Unit
              ): Unit = {
    if (reportEnabled(verbosity, SuvmSeverity.UVM_INFO, id)) {
      reportInfo(id, msg, verbosity, "", 0, "", true)
    }
  }

  def suvmWarning(id: String,
               msg: String,
               reportEnabled: (SuvmVerbosity.Value, SuvmSeverity.Value, String) => Boolean,
               reportWarn: (String, String, SuvmVerbosity.Value, String, Int, String, Boolean) => Unit
              ): Unit = {
    if (reportEnabled(SuvmVerbosity.UVM_NONE, SuvmSeverity.UVM_WARNING, id)) {
      reportWarn(id, msg, SuvmVerbosity.UVM_NONE, "", 0, "", true)
    }
  }
}
