trait Base
object Hello1 extends Base
object Hello2 extends Base

val a: Base = Hello1
val b: Base = Hello2
val c: Base = Hello1
a match {
  case c => println("match b")
  case _ => println("match c")
}