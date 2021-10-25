package dev

import java.io.{File, FileWriter}
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Misc extends App {
  val p = Promise[Unit]

  def sUvmWait(): Unit = {
    Await.result(p.future, Duration.Inf)
    println("Done!!!")
  }
  def trigger(): Unit = p.trySuccess()

  Future {
    Thread.sleep(1000)
    trigger()
  }

  sUvmWait()
}
