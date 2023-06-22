package uvm.chiter

import uvm._

import java.util.Calendar
import scala.collection.mutable

trait ChiterBackend {
  this: ChiterThreadBackend with ChiterSimulator =>

  private def timeDuration(start: BigInt): Float = {
    val end = Calendar.getInstance().getTimeInMillis
    (end - start).toFloat / 1000.0F
  }

  def backendRun[T <: ChiterHarness](fn: T => Unit): Unit = {
    val start = Calendar.getInstance().getTimeInMillis
    try {
      mainThreadStart(fn)
      println(s"Test Finished @$getTimeNow in ${timeDuration(start)}s!")
    } catch {
      case TestFinishedException(t) =>
        println(s"Test Finished @$t in ${timeDuration(start)}s!")
      case e@(_: Exception | _: Error) => throw e
    }
  }

  private def updateClocks(): Unit = {
    clocks.foreach { i =>
      i.periodLeft -= 1
      if (i.periodLeft == 0 || i.periodLeft == i.period / 2) {
        poke(i.clk, if (peek(i.clk) == 0) 1 else 0)
      }
      if (i.periodLeft == 0) {
        i.periodLeft = i.period
      }
    }
  }

  private def mainThreadStart[T <: ChiterHarness](testFn: T => Unit): Unit = {
    clocks.foreach { i => poke(i.clk, 0) }

    val mainThread = createThread("mainThread", () => {
      poke(dut.get.reset, 1)
      doWait(1)
      poke(dut.get.reset, 0)
      testFn(dut.get.asInstanceOf[T])
    }, None)

    mainThread.thread.start()
    blockedThreads.update(0, mutable.ListBuffer(mainThread))

    try {
      clocks.foreach(i => poke(i.clk, if (i.periodLeft > i.period / 2) 1 else 0))

      while (!mainThread.isDone(false)) {
        val timeNow = getTimeNow
        debugLog(s"clock $timeNow")
        updateClocks()
        update()

        val threads = mutable.ListBuffer.empty[ChiterThread]
        threads ++= blockedThreads.getOrElse(timeNow, Seq())
        blockedThreads.remove(timeNow)

        runThreads(threads.toSeq)

        zombieThreads.foreach { i =>
          if (i.thread.isAlive) {
            debugLog(s"thread killed: ${i.name}")
            i.thread.interrupt()
          }
        }
        zombieThreads.clear()
        step(1)
        if (timeout != 0 && getTimeNow > timeout)
          uvmFatal("CHITER", s"simulation time expire @$timeout")
      }
    } finally {
      allThreads.foreach { i =>
        if (i.thread.isAlive) {
          i.thread.interrupt()
        }
      }
    }
  }

  @annotation.tailrec
  final def doWait(condition: => Boolean): Unit = {
    val currentThread = current.get
    if (!condition) {
      deferThreads += currentThread
      scheduler()
      currentThread.waiting.acquire()
      doWait(condition)
    } else {
      deferSize = -1
    }
  }

  def doWait(cycles: Int): Unit = {
    val currentThread = current.get
    if (cycles == 0) {
      activeThreads += currentThread
    } else {
      val deferTime = getTimeNow + cycles
      blockedThreads.getOrElseUpdate(deferTime, mutable.ListBuffer.empty[ChiterThread]) += currentThread
    }
    scheduler()
    currentThread.waiting.acquire()
  }
}
