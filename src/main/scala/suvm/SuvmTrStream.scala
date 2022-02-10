package suvm

abstract class SuvmTrStream(name: String) extends SuvmObject(name) {
  def getDb: SuvmTrDatabase

  def openRecorder(name: String, openTime: Time, typeName: String): Option[SuvmRecorder]
}
