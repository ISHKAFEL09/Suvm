package chiseltester

import chisel3._
import treadle._

import scala.collection.mutable

private[chiseltester] class TreadleBackend[T <: Module](dut: T,
                                                        names: Map[Data, String],
                                                        paths: Map[Data, Set[Data]],
                                                        tester: TreadleTester)
  extends BackendInstance[T] with ThreadedBackend {

  val clockCounter = mutable.HashMap.empty[Clock, Int]
  val lastClockValue = mutable.HashMap.empty[Clock, Boolean]

  def getClockCycle(clk: Clock): Int = {
    clockCounter.getOrElse(clk, 0)
  }

  def getClock(clk: Clock): Boolean = tester.peek(names(clk)).toInt match {
    case 0 => false
    case 1 => true
  }

  def resolveName(signal: Data): String = {
    names.getOrElse(signal, signal.toString)
  }

  def getModule: T = dut

  override def run(testFn: T => Unit): Unit = {
    val mainThread = new TesterThread("mainThread", () => {
      tester.poke("reset", 1)
      tester.step(1)
      tester.poke("reset", 0)
      testFn(dut)
    }, None)
    mainThread.thread.start()

    val blockedThreads = mutable.HashMap.from(Seq(dut.clock -> mutable.ListBuffer(mainThread)))
    try {
      while (!mainThread.done) {
        val threads = mutable.ListBuffer.empty[TesterThread]
        threads ++= blockedThreads.getOrElse(dut.clock, Seq())
        blockedThreads.remove(dut.clock)
        clockCounter.put(dut.clock, getClockCycle(dut.clock) + 1)
        lastClockValue.foreach { case (clock, lastValue) =>
          val currentValue = getClock(clock)
          if (currentValue != lastValue) {
            lastClockValue.put(clock, currentValue)
            if (currentValue) {
              threads ++= blockedThreads.getOrElse(clock, Seq())
              blockedThreads.remove(clock)
              clockCounter.put(clock, getClockCycle(clock) + 1)
            }
          }
        }
        debugLog(s"clock ${getClockCycle(dut.clock)}")
        runThreads(threads).foreach { case (clk, ts) =>
          blockedThreads.getOrElseUpdate(clk, mutable.ListBuffer.empty[TesterThread]) ++= ts
        }
        zombieThreads.foreach { i =>
          if (i.thread.isAlive) {
            debugLog(s"thread killed: ${i.name}")
            i.thread.interrupt()
          }
        }
        zombieThreads.clear()
        Context().env.checkpoint()
        tester.step(1)
      }
    } finally {
      allThreads.foreach { i =>
        if (i.thread.isAlive) {
          i.thread.interrupt()
        }
      }
      tester.report()
    }
  }

  override def pokeBits(signal: Bits, value: BigInt): Unit = {
    tester.poke(names(signal), value)
    debugLog(s"${resolveName(signal)} <- $value")
  }

  override def peekBits(signal: Bits, stale: Boolean): BigInt = {
    require(!stale)

    val value = tester.peek(names(signal))
    debugLog(s"${resolveName(signal)} -> $value")
    value
  }

  override def step(signal: Clock, cycles: Int): Unit = {
    for (_ <- 0 until cycles) {
      if (signal != dut.clock && !lastClockValue.contains(signal)) {
        lastClockValue.put(signal, getClock(signal))
      }

      val currentThread = current.get
      blockedThreads.getOrElseUpdate(signal, mutable.ListBuffer.empty[TesterThread]) += currentThread
      scheduler()
      currentThread.waiting.acquire()
    }
  }

  override def getMainClock: Clock = getModule.clock

  @annotation.tailrec
  final override def doWait(condition: => Boolean): Unit = {
    val currentThread = current.get
    if (!condition) {
      if (activeThreads.exists(!_.waitFlag)) {
        currentThread.waitFlag = true
        activeThreads += currentThread
        scheduler()
        currentThread.waiting.acquire()
      } else {
        currentThread.waitFlag = false
        step(dut.clock, 1)
      }
      doWait(condition)
    }
  }

  override def doWait(cycles: Int): Unit = {
    if (cycles == 0) {
      val currentThread = current.get
      activeThreads += currentThread
      scheduler()
      currentThread.waiting.acquire()
    } else
      step(dut.clock, cycles)
  }
}
