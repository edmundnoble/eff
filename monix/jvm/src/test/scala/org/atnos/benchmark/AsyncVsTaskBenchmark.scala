package org.atnos.benchmark

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.atnos.eff._
import org.atnos.eff.asyncmonix.AsyncTaskInterpreter
import org.atnos.eff.syntax.all._
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import scala.concurrent.Await
import scala.concurrent.duration._

object AsyncVsTaskBenchmark extends Bench.OfflineReport {
  type A = Fx.fx1[Async]
  type T = Fx.fx1[Task]

  def loopA(i: Int): Task[Eff[A, Int]] =
    if (i == 0) Task.now(Eff.pure(1))
    else Task.now(AsyncTaskInterpreter.suspend(loopA(i - 1)).map(_ * i))

  def loopT(i: Int): Task[Eff[T, Int]] =
    if (i == 0) Task.now(Eff.pure(1))
    else Task.now(Eff.send[Task, T, Eff[T, Int]](Task.suspend(loopT(i - 1))).flatten[Int].map(_ * i))

  val sizes = Gen.enumeration("size")(10, 100, 1000, 10000, 100000)

  performance of "trampolined recursion" in {
    measure method "task" in {
      using(sizes) in { size =>
        Await.result(Eff.send[Task, T, Eff[T, Int]](loopT(size)).flatten[Int].detach(AsyncTaskInterpreter.TaskMonad).runAsync, 5 seconds)
      }
    }
    measure method "async" in {
      using(sizes) in { size =>
        Await.result(AsyncTaskInterpreter.run(AsyncTaskInterpreter.suspend(loopA(size)).detach).runAsync, 5 seconds)
      }
    }
  }


}
