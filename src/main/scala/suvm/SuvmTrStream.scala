package suvm

import SuvmImplicits._

class SuvmTrStream(val name: String) extends SuvmObject {
  def getDb: SuvmTrDatabase = ???

  def openRecorder(name: String, openTime: Time = 0.s, typeName: String = ""): Option[SuvmRecorder] = ???
}
