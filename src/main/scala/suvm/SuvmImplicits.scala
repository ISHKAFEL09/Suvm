package suvm

import suvm.SuvmObjectGlobals._

import scala.annotation.tailrec
import scala.language.implicitConversions

object SuvmImplicits {
  /**
   * implicit conversation
   */
  implicit def Int2Time(x: Int): SuvmTime = new SuvmTime(x.toDouble)

  implicit def Double2Time(x: Double): SuvmTime = new SuvmTime(x)

  implicit def Time2BigInt(x: Time): BigInt = BigInt((x.value * x.unit.unit).toInt)

  implicit def Int2BigInt(x: Int): BigInt = BigInt(x)

  /**
   * type class
   */
  trait FieldOps[T <: Enumeration#Value] {
    def op: T
    def create(i: Int): T
    def |(x: T): T = create(op.id | x.id)
    def &(x: T): T = create(op.id & x.id)
    def hasOp(i: T): Boolean = (op.id & i.id) != 0
  }

  implicit class SuvmOpcodeOps(val op: SuvmOpcodeEnum.Value) extends FieldOps[SuvmOpcodeEnum.Value] {
    override def create(i: Int): SuvmOpcodeEnum.Value = new SuvmOpcodeEnum.Value {
      override def id: Int = i
    }
  }

  implicit class SuvmActionOps(val op: SuvmActionType.Value) extends FieldOps[SuvmActionType.Value] {
    override def create(i: Int): SuvmActionType.Value = new SuvmActionType.Value {
      override def id: Int = i
    }
  }
}
