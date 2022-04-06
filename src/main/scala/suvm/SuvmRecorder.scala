package suvm

import SuvmObjectGlobals._

abstract class SuvmRecorder extends SuvmPolicy {
  import SuvmImplicits._

  def getHandle: Int = ???

  def close(closeTime: Time = 0.s): Unit = ???

  def free(closeTime: Time = 0.s): Unit = ???

  def recordField(name: String,
                  value: SuvmBitstream,
                  size: Int,
                  radix: SuvmRadixEnum.Value = SuvmRadixEnum.UVM_NORADIX): Unit =
    println(s"$name: $value")

  def setRecursionPolicy(p: SuvmRecursionPolicy.Value): Unit = policy = p

  def getRecursionPolicy: SuvmRecursionPolicy.Value = policy

  def recordObject(name: String, value: SuvmObject): Unit = {
    println(s"$name: $value")
  }

  private var policy: SuvmRecursionPolicy.Value = SuvmRecursionPolicy.UVM_DEFAULT_POLICY
}

object SuvmRecorder {
  def getRecorderFromHandle(id: Int): Option[SuvmRecorder] = ???
}