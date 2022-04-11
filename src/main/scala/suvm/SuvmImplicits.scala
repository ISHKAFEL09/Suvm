package suvm

import suvm.SuvmObjectGlobals._

import java.io.File
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

  implicit def Enum2Int(x: Enumeration#Value): Int = x.id

  implicit def File2BitStream(x: Option[File]): SuvmBitstream = x match {
    case Some(value) => value.hashCode()
    case None => 0
  }

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

  implicit class SuvmActionOps(val op: SuvmAction.Value) extends FieldOps[SuvmAction.Value] {
    override def create(i: Int): SuvmAction.Value = new SuvmAction.Value {
      override def id: Int = i
    }
  }
}
