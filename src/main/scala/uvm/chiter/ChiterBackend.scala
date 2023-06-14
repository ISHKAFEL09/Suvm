package uvm.chiter

import uvm._
import scala.collection.mutable

trait ChiterBackend {
  this: ChiterThreadBackend with ChiterSimulator =>

  def backendRun[T <: ChiterHarness](fn: T => Unit): Unit = {
    try {
      mainThreadStart(fn)
    } catch {
      case TestFinishedException =>
      case e@(_: Exception | _: Error) => throw e
    } finally {
      println(s"Test Finished @$getTimeNow!")
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
      println(s"period left: ${i.periodLeft}, period: ${i.period}")
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
      clocks.foreach(i => poke(i.clk, if (i.periodLeft >= i.period / 2) 0 else 1))

      while (!mainThread.isDone(false)) {
        val timeNow = getTimeNow
        debugLog(s"clock $timeNow")
        updateClocks()
        println(blockedThreads.keys, blockedThreads.values.map(_.size))

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
      if (activeThreads.exists(!_.defer)) {
        currentThread.defer = true
        activeThreads += currentThread
        scheduler()
        currentThread.waiting.acquire()
      } else {
        currentThread.defer = false
        doWait(1)
      }
      doWait(condition)
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
