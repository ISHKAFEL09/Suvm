package chiter

import java.util.concurrent._
import scala.collection.mutable
import chisel3._

trait ChiterThreadBackend {
  var current: Option[ChiterThread] = None
  val blockedThreads = mutable.HashMap.empty[BigInt, mutable.ListBuffer[ChiterThread]]
  val allThreads: mutable.ArrayBuffer[ChiterThread] = mutable.ArrayBuffer.empty[ChiterThread]
  val joinedThreads = mutable.HashMap.empty[ChiterThread, Seq[ChiterThread]]
  val activeThreads: mutable.ListBuffer[ChiterThread] = mutable.ListBuffer.empty[ChiterThread]
  val zombieThreads: mutable.ListBuffer[ChiterThread] = mutable.ListBuffer.empty[ChiterThread]
  val clocks: mutable.ListBuffer[ClockInfo] = mutable.ListBuffer.empty[ClockInfo]

  def addClock(clk: Clock, period: BigInt): Unit = clocks += ClockInfo(clk, period)

  def createThread(name: String, runnable: () => Unit, parent: Option[ChiterThread]): ChiterThread

  def doFork(name: String, runnable: () => Unit): ChiterThread

  def runThreads(threads: Seq[ChiterThread]): Unit

  def scheduler(): Unit
}

trait ChiterMultiThreadBackend extends ChiterThreadBackend {
  private val interruptedException = new ConcurrentLinkedQueue[Throwable]()

  private def onException(e: Throwable): Unit = interruptedException.offer(e)

  private class TesterThread(val name: String = "thread",
                             runnable: () => Unit,
                             parent: Option[ChiterThread]) extends ChiterThread { id =>

    val children: mutable.ListBuffer[ChiterThread] =
      collection.mutable.ListBuffer.empty[ChiterThread]

    parent.foreach(_.children += this)

    var done: Boolean = false
    var killFlag: Boolean = false

    val thread: Thread = new Thread(() => {
      try {
        waiting.acquire()
        runnable()
        clear()
        scheduler()
        debugLog(s"thread done: $name")
      } catch {
        case _: InterruptedException =>
        case e@(_: Exception | _: Error) =>
          onException(e)
          scheduler()
      }
    })
    debugLog(s"thread created: $name")

    def isDone(deep: Boolean = true): Boolean = {
      done && (!deep || children.forall(_.isDone(deep)))
    }

    override def kill(): Unit = {
      children.foreach(_.kill())
      killFlag = true
    }

    def clear(): Unit = {
      done = true
      threadFinished(id)
    }
  }

  private val mainSemaphore: Semaphore = new Semaphore(0)

  override def runThreads(threads: Seq[ChiterThread]): Unit = {
    require(activeThreads.isEmpty)

    activeThreads ++= threads.map(_.asInstanceOf[TesterThread])
    scheduler()
    mainSemaphore.acquire()

    if (!interruptedException.isEmpty) {
      throw interruptedException.poll()
    }

    require(activeThreads.isEmpty)
  }

  @annotation.tailrec
  final def scheduler(): Unit = {
    if (activeThreads.isEmpty) {
      current = None
      mainSemaphore.release()
    } else {
      val nextThread = activeThreads.remove(0).asInstanceOf[TesterThread]
      if (!nextThread.killFlag) {
        current = Some(nextThread)
        nextThread.waiting.release()
      } else {
        nextThread.clear()
        zombieThreads += nextThread
        scheduler()
      }
    }
  }

  private def threadFinished(thread: TesterThread): Unit = {
    allThreads -= thread
    joinedThreads.remove(thread) match {
      case Some(threads) => activeThreads ++= threads
      case None =>
    }
  }

  override def createThread(name: String, runnable: () => Unit, parent: Option[ChiterThread]): ChiterThread =
    new TesterThread(name, runnable, parent)

  override def doFork(name: String, runnable: () => Unit): ChiterThread = {
    val currentThread = current.get
    val newThread = new TesterThread(name, runnable = runnable, parent = Some(currentThread))
    allThreads += newThread
    activeThreads.prependAll(Seq(newThread, currentThread.asInstanceOf[TesterThread]))
    newThread.thread.start()
    scheduler()
    currentThread.waiting.acquire()
    newThread
  }
}
