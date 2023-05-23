import chisel3._
import chisel3.reflect.DataMirror
import firrtl.AnnotationSeq

import scala.annotation.tailrec
import scala.util.DynamicVariable

package object chiseltester {
  var verbosity = true

  def debugLog(str: => String): Unit = {
    if (verbosity)
      println(s"[DEBUG LOG] $str")
  }

  object Context {
    case class Instance(backend: BackendInterface, env: TestEnvInterface)

    private val context = new DynamicVariable[Option[Instance]](None)

    def run[T <: Module](backend: BackendInstance[T],
                         env: TestEnvInterface,
                         testFn: T => Unit): Unit = {
      require(context.value.isEmpty)
      context.withValue(Some(Instance(backend, env))) {
        backend.run(testFn)
      }
    }

    def createDefaultTester[T <: Module](dutGen: () => T,
                                         annotationSeq: AnnotationSeq): BackendInstance[T] = {
      TreadleExecutive.start(dutGen, annotationSeq)
    }

    def apply(): Instance = context.value.get
  }

  case class NotLiteralException(message: String) extends Exception(message)
  case class LiteralTypeException(message: String) extends Exception(message)
  case class UnpokeableException(message: String) extends Exception(message)

  implicit class testableData[T <: Data](x: T) {
    def pokeBits(signal: Bits, value: BigInt): Unit = {
      if (DataMirror.directionOf(signal) != ActualDirection.Input) {
        throw new UnpokeableException("Can only poke inputs")
      }
      Context().backend.pokeBits(signal, value)
    }

    def poke(value: T): Unit = (x, value) match {
      case (x: Bool, value: Bool) => pokeBits(x, value.litValue)
      case (x: Bits, value: UInt) => pokeBits(x, value.litValue)
      case (x: SInt, value: SInt) => pokeBits(x, value.litValue)
      case (x: Bundle, value: Bundle) => x.elements zip value.elements foreach {
        case ((_, x), (_, value)) => x.poke(value)
      }
      case x => throw LiteralTypeException(s"don't know how to poke $x")
    }

    def peekBits(stale: Boolean): T = x match {
      case x: Bool => Context().backend.peekBits(x, stale) match {
        case x: BigInt if x == 0 => false.B.asInstanceOf[T]
        case x: BigInt if x == 1 => true.B.asInstanceOf[T]
        case x => throw LiteralTypeException(s"peeked Bool with value $x not 0 or 1")
      }
      case x: UInt => Context().backend.peekBits(x, stale).asUInt(x.getWidth.W).asInstanceOf[T]
      case x: SInt => Context().backend.peekBits(x, stale).asSInt(x.getWidth.W).asInstanceOf[T]
      case x => throw LiteralTypeException(s"don't know how to peek $x")
    }

    def peek(): T = peekBits(false)
  }

  implicit class testableClock(x: Clock) {
    def step(n: Int = 1): Unit = Context().backend.step(x, n)
  }

  def fork(name: String = "thread")(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(Seq(Context().backend.doFork(name, () => runnable)))
  }

  def parallel(run1: => Unit, run2: => Unit): Unit = {
    fork()(run1).fork()(run2).join()
  }

  def ~>(condition: => Boolean): Unit = {
    Context().backend.doWait(condition)
  }

  def ~>(cycles: Int): Unit = {
    Context().backend.doWait(cycles)
  }

  def ~>(event: Event): Unit = {
    ~>(event.isTriggered)
    debugLog(s"event ${event.name} triggered")
    ~>(0)
  }
}
