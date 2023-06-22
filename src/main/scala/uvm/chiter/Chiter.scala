package uvm.chiter

import chisel3._
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import treadle.WriteVcdAnnotation
import uvm._

import java.util.concurrent.Semaphore

trait ChiterThread {
  def thread: Thread

  def isDone(deep: Boolean = true): Boolean

  def kill(): Unit

  def name: String

  def children: collection.mutable.ListBuffer[ChiterThread]

  val waiting: Semaphore = new Semaphore(0)
}

trait ChiterThreadList {
  def toSeq: Seq[ChiterThread]

  def join(): Unit

  def joinAny(): Unit

  def ++(others: ChiterThreadList): ChiterThreadList

  def fork(name: String = "thread")(runnable: => Unit): ChiterThreadList

  def kill(): Unit

  def done: Boolean
}

trait Event {
  val name: String

  def trigger(): Unit

  def isTriggered: Boolean
}

trait Chiter[T <: ChiterHarness] {
  this: ChiterBackend with ChiterSimulator with ChiterThreadBackend =>

  def createEvent(name: String): Event = _Event(name)

  private case class _Event(name: String = "Event") extends Event {
    private var mTrig = false

    override def trigger(): Unit = {
      mTrig = true
      debugLog(s"trigger event $name")
      ~>(1)
      mTrig = false
    }

    override def isTriggered: Boolean = mTrig
  }

  private class _ThreadList(elements: Seq[ChiterThread]) extends ChiterThreadList {
    def toSeq: Seq[ChiterThread] = elements

    def join(): Unit = ~>(done)

    def joinAny(): Unit = ~>(elements.exists(_.isDone()))

    def ++(others: ChiterThreadList): ChiterThreadList = {
      new _ThreadList(elements ++ others.toSeq)
    }

    def fork(name: String = "thread")(runnable: => Unit): ChiterThreadList = {
      new _ThreadList(elements :+ doFork(name, () => runnable))
    }

    def kill(): Unit = {
      elements.foreach(_.kill())
      ~>(done)
    }

    def done: Boolean = elements.forall(_.isDone())
  }

  def harness(): T

  def top: T => UVMTest

  def compile: Boolean = true

  def annotations: AnnotationSeq = AnnotationSeq(Seq(
    TargetDirAnnotation("test_run_dir"),
    WriteVcdAnnotation)
  )

  def run(body: T => Unit): Unit = {
    build(harness, annotations, compile)
    dut.get.clocks.foreach(addClock)
    backendRun(body)
  }

  def ~>(condition: => Boolean): Unit = doWait(condition)

  def ~>(cycles: Int): Unit = doWait(cycles)

  def ~>(event: Event): Unit = {
    ~>(event.isTriggered)
    debugLog(s"event ${event.name} triggered")
    ~>(0)
  }

  def ~>(clk: Clock, posedge: Boolean): Unit = {
    clocks.find(_.clk == clk) match {
      case Some(clkInfo) =>
        if (posedge)
          ~>(clkInfo.periodLeft == clkInfo.period)
        else
          ~>(clkInfo.periodLeft == (clkInfo.period / 2))
      case None =>
        uvmFatal("CHITER", s"Register clock ${clk.name} with clock() first!")
    }
  }

  def fork(name: String = "thread")(runnable: => Unit): ChiterThreadList = {
    new _ThreadList(Seq(doFork(name, () => runnable)))
  }

  def getCurrent: ChiterThread = current.get

  def finish(status: FinishStatus): Unit = {
    val t = getTimeNow
    simFinish()
    status match {
      case SuccessStatus =>
      case StopStatus =>
        println(s"finish() called in ${new Throwable().getStackTrace()(4).toString}")
      case FatalStatus =>
        assert(cond = false, "Fatal!!!")
    }
    throw TestFinishedException(t)
  }
}
