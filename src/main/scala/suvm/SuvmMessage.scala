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
}
