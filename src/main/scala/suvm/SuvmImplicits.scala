package suvm

import suvm.SuvmObjectGlobals._

import scala.language.implicitConversions

object SuvmImplicits {
  implicit def Int2Time(x: Int): SuvmTime = new SuvmTime(x.toDouble)

  implicit def Double2Time(x: Double): SuvmTime = new SuvmTime(x)

  trait FieldOps[T] {
    def |(x: T): T
    def &(x: T): T
  }

  implicit class SuvmOpcodeOps(op: SuvmOpcodeEnum.Value) extends FieldOps[SuvmOpcodeEnum.Value] {
    def |(x: SuvmOpcodeEnum.Value): SuvmObjectGlobals.SuvmOpcodeEnum.Value = new SuvmOpcodeEnum.Value {
      override def id: Int = op.id | x.id
    }

    def &(x: SuvmOpcodeEnum.Value): SuvmObjectGlobals.SuvmOpcodeEnum.Value = new SuvmOpcodeEnum.Value {
      override def id: Int = op.id & x.id
    }
  }

  implicit def Time2BigInt(x: Time): BigInt = BigInt((x.value * x.unit.unit).toInt)
}
