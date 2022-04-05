import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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

val s = "a\nb"
println(s)

object ThreadTest {
  object Top {
    var done = false

    def run = {
      println("waiting done...")
      Future {
        inner
      }
      while (!done) {}
      println("done")
    }
  }

  def inner = {
    println("sleeping...")
    Thread.sleep(1000)
    Top.done = true
    println("sleeping done")
  }
}

ThreadTest.Top.run