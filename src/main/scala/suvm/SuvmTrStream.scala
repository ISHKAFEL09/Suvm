package suvm

abstract class SuvmTrStream extends SuvmObject {
  def getDb: SuvmTrDatabase

  def openRecorder(name: String, openTime: Time, typeName: String): Option[SuvmRecorder]
}
