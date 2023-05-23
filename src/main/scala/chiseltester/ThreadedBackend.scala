package chiseltester

import chisel3._

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}
import scala.collection._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class Event(name: String = "Event") extends AbstractEvent {
  private var mTrig = false

  override def trigger(): Unit = {
    mTrig = true
    debugLog(s"trigger event $name")
    ~>(0)
    mTrig = false
  }

  override def isTriggered: Boolean = mTrig
}

class TesterThreadList(elements: Seq[AbstractTesterThread]) {
  def toSeq: Seq[AbstractTesterThread] = elements

  def join(): Unit = ~>(done)

  def joinAny(): Unit = ~>(elements.exists(_.isDone))

  def ++(others: TesterThreadList): TesterThreadList = {
    new TesterThreadList(elements ++ others.toSeq)
  }

  def fork(name: String = "thread")(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(elements :+ Context().backend.doFork(name, () => runnable))
  }

  def kill(): Unit = {
    elements.foreach(_.kill())
    ~>(done)
  }

  def done: Boolean = elements.forall(_.isDone)
}

private[chiseltester] trait ThreadedBackend {
  val interruptedException = new ConcurrentLinkedQueue[Throwable]()
  def onException(e: Throwable): Unit = interruptedException.offer(e)

  class TesterThread(val name: String = "thread",
                     runnable: () => Unit,
                     parent: Option[TesterThread]) extends AbstractTesterThread { id =>
    private val children: ListBuffer[AbstractTesterThread] = collection.mutable.ListBuffer.empty[AbstractTesterThread]
    val level: Int = parent match {
      case Some(value) =>
        value.children += this
        value.level + 1
      case None => 0
    }
    val waiting: Semaphore = new Semaphore(0)
    var done: Boolean = false
    var killFlag: Boolean = false
    var waitFlag: Boolean = false

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

    def isDone: Boolean = {
      done && children.forall(_.isDone)
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

  var current: Option[TesterThread] = None
  val mainSemaphore: Semaphore = new Semaphore(0)
  val allThreads: ArrayBuffer[TesterThread] = mutable.ArrayBuffer.empty[TesterThread]
  val joinedThreads = mutable.HashMap.empty[TesterThread, Seq[TesterThread]]
  val activeThreads: ListBuffer[TesterThread] = mutable.ListBuffer.empty[TesterThread]
  val blockedThreads = mutable.HashMap.empty[Clock, mutable.ListBuffer[TesterThread]]
  val zombieThreads: ListBuffer[TesterThread] = mutable.ListBuffer.empty[TesterThread]

  def runThreads(threads: Seq[TesterThread]): Map[Clock, Seq[TesterThread]] = {
    require(activeThreads.isEmpty)
    require(blockedThreads.isEmpty)

    activeThreads ++= threads
    scheduler()
    mainSemaphore.acquire()

    if (!interruptedException.isEmpty) {
      throw interruptedException.poll()
    }

    require(activeThreads.isEmpty)
    val rtn = blockedThreads.map { case (c, s) => (c, s.toSeq) }.toMap
    blockedThreads.clear()
    rtn
  }

  @annotation.tailrec
  final def scheduler(): Unit = {
    if (activeThreads.isEmpty) {
      current = None
      mainSemaphore.release()
    } else {
      val nextThread = activeThreads.remove(0)
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

  def threadFinished(thread: TesterThread): Unit = {
    allThreads -= thread
    joinedThreads.remove(thread) match {
      case Some(threads) => activeThreads ++= threads
      case None =>
    }
  }

  def doFork(name: String, runnable: () => Unit): TesterThread = {
    val currentThread = current.get
    val newThread = new TesterThread(name, runnable = runnable, parent = Some(currentThread))
    allThreads += newThread
    activeThreads.prependAll(Seq(newThread, currentThread))
    newThread.thread.start()
    scheduler()
    currentThread.waiting.acquire()
    newThread
  }

  def doJoin(thread: AbstractTesterThread): Unit = {
    val currentThread = current.get
    val newThread = thread.asInstanceOf[TesterThread]
    require(currentThread.level < newThread.level)
    if (!newThread.done) {
      joinedThreads.put(newThread, joinedThreads.getOrElseUpdate(newThread, Seq()) :+ currentThread)
      scheduler()
      currentThread.waiting.acquire()
    }
  }
}
