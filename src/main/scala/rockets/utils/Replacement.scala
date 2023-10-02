package rockets.utils

import spinal.core._

/**
 * replacement policy, lru, random...?
 */
trait Replacement {
  /**
   * @return the next position
   */
  def get: UInt

  /**
   * @param way current used position, used to update inner state
   */
  def set(way: UInt): Unit
}

/**
 * fake LRU replacement
 * @param n total position number
 */
case class PLRU(n: Int) extends Replacement {
  /**
   * inner state, used to generate next position
   */
  val state: UInt = RegInit(U(0, n bits)).setName("plru_state")

  /**
   *  @return the next position based on state
   */
  override def get: UInt = {
    var idx = U(1, 1 bits)
    for (_ <- 0 until log2Up(n))
      idx = idx @@ state(idx)
    idx(0 until log2Up(n))
  }

  /**
   * @param way current used position, used to update inner state
   */
  override def set(way: UInt): Unit = {
    var nextState = state
    var idx = U(1, 1 bits)
    for (i <- log2Up(n) - 1 to 0 by -1) {
      val mask = U(1, n bits) |<< idx
      nextState = nextState & ~mask | Mux(way(i), U(0).resized, mask)
      idx = idx @@ way(i)
    }
    state := nextState
  }
}