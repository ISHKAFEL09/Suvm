package chiseltester

import chisel3._
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import org.scalatest._
import org.scalatest.exceptions.TestFailedException

import scala.collection.mutable.ArrayBuffer

trait ChiselTester extends Assertions with TestSuiteMixin with TestEnvInterface {
  this: TestSuite =>

  protected val batchedFailures = ArrayBuffer.empty[TestFailedException]

  override def testerFail(msg: String): Unit = {
    batchedFailures += new TestFailedException(s"$msg", 4)
  }

  override def checkpoint(): Unit = {
    batchedFailures.foreach(throw _)
  }

  def addDefaultAnnos: AnnotationSeq = {
    AnnotationSeq(Seq(
      TargetDirAnnotation("test_run_dir/chiseltester"),
      WriteVcdAnnotation)
    )
  }

  def test[T <: Module](dutGen: => T)(testFn: T => Unit): Unit = {
    val tester = Context.createDefaultTester(() => dutGen, addDefaultAnnos)
    try {
      Context.run(tester, this, testFn)
    } catch {
      case TestFinishedException => println("Test Finished!")
      case e@(_: Exception | _: Error) => throw e
    }
  }
}
