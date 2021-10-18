package dev

import java.util.concurrent.CancellationException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise, blocking}

class SuvmFork {

}

object SuvmFork extends App{
  type Cancellable[T] = (Promise[Unit], Future[T])
  private def cancellable[T](f: Future[Unit] => T): Cancellable[T] = {
    val p = Promise[Unit]
    val value = Future {
      val r = blocking(f(p.future))
      if (!p.tryFailure(new Exception))
        throw new CancellationException
      r
    }
    (p, value)
  }

  private implicit class FutureOps[T](self: Future[T]) {
    def or(that: Future[T]): Future[T] = {
      val p = Promise[T]
      self onComplete(i => p tryComplete i)
      that onComplete(i => p tryComplete i)
      p.future
    }

    def and(that: Future[T]): Future[T] = {
      val p = Promise[T]
      self onComplete(i => if (that.isCompleted) p tryComplete i)
      that onComplete(i => if (self.isCompleted) p tryComplete i)
      p.future
    }
  }

  private def suvmFork(works: (Future[Unit] => Unit)*)(f: (Future[Unit], Future[Unit]) => Future[Unit])(wait: Boolean): Unit = {
    val (cancel, value) = works.map(i => cancellable(i)).unzip
    val v = value reduce f
    if (wait) Await.result(v, Duration.Inf)
    cancel foreach(i => i.trySuccess())
  }
  def suvmForkJoin(works: (Future[Unit] => Unit)*): Unit = suvmFork(works: _*)((i, j) => i.and(j))(wait = true)
  def suvmForkAny(works: (Future[Unit] => Unit)*): Unit = suvmFork(works: _*)((i, j) => i.or(j))(wait = true)
  def suvmForkNone(works: (Future[Unit] => Unit)*): Seq[Cancellable[Unit]] = works.map(i => cancellable(i))

  /* example
  suvmForkJoin (
    (_: Future[Unit]) => {
      var i = 0
      while (i < 5) {
        Thread.sleep(100)
        i += 1
      }
      println("fork join: job 1 done!!!")
    },
    (_: Future[Unit]) => {
      var i = 0
      while (i < 5) {
        Thread.sleep(300)
        i += 1
      }
      println("fork join: job 2 done!!!")
    },
    (_: Future[Unit]) => {
      var i = 0
      while (i < 5) {
        Thread.sleep(200)
        i += 1
      }
      println("fork join: job 3 done!!!")
    }
  )

  suvmForkAny (
    (cancel: Future[Unit]) => {
      while (!cancel.isCompleted) {
        Thread.sleep(100)
      }
      println("fork any: infinity job done!!!")
    },
    (_: Future[Unit]) => {
      var i = 0
      while (i < 5) {
        Thread.sleep(300)
        i += 1
      }
      println("fork any: 300ms!!!")
    },
    (_: Future[Unit]) => {
      var i = 0
      while (i < 5) {
        Thread.sleep(200)
        i += 1
      }
      println("fork any: 200ms!!!")
    }
  )

  val job = suvmForkNone (
    (cancel: Future[Unit]) => {
      while (!cancel.isCompleted) {
        Thread.sleep(100)
      }
      println("fork any: infinity job 1 done!!!")
    },
    (cancel: Future[Unit]) => {
      while (!cancel.isCompleted) {
        Thread.sleep(100)
      }
      println("fork any: infinity job 2 done!!!")
    },
    (cancel: Future[Unit]) => {
      while (!cancel.isCompleted) {
        Thread.sleep(100)
      }
      println("fork any: infinity job 3 done!!!")
    }
  )
  job.map(_._1).foreach(i => i.trySuccess())

  Thread.sleep(100)
   */
}
