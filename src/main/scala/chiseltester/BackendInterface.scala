package chiseltester

import chisel3._

trait AbstractTesterThread {
  def thread: Thread

  def isDone: Boolean

  def kill(): Unit
}

trait BackendInterface {
  def pokeBits(signal: Bits, value: BigInt): Unit

  def peekBits(signal: Bits, stale: Boolean): BigInt

  def step(signal: Clock, cycles: Int): Unit

  def doFork(name: String, runnable: () => Unit): AbstractTesterThread

  def doJoin(thread: AbstractTesterThread): Unit

  def doWait(condition: => Boolean): Unit

  def doWait(cycles: Int): Unit

  def getMainClock: Clock
}

trait BackendInstance[T <: Module] extends BackendInterface {
  def run(testFn: T => Unit): Unit
}

trait TestEnvInterface {
  def testerFail(msg: String): Unit

  def checkpoint(): Unit
}