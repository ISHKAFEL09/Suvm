package chiseltester

import chisel3._

trait AbstractTesterThread {
  def thread: Thread
}

trait BackendInterface {
  def pokeBits(signal: Bits, value: BigInt): Unit

  def peekBits(signal: Bits, stale: Boolean): BigInt

  def step(signal: Clock, cycles: Int): Unit

  def doFork(runnable: () => Unit): AbstractTesterThread

  def doJoin(thread: AbstractTesterThread): Unit

  def getMainClock: Clock
}

trait BackendInstance[T <: Module] extends BackendInterface {
  def run(testFn: T => Unit): Unit
}

trait TestEnvInterface {
  def testerFail(msg: String): Unit

  def checkpoint(): Unit
}