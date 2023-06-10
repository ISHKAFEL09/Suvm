package object chiter {
  def debugLog(s: String): Unit =
    println(s"[DEBUG LOG] $s")

  object TestFinishedException extends Exception("Test Finished!")
}
