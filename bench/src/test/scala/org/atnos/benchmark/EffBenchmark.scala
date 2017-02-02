package org.atnos.benchmark

import org.scalameter.api._
import org.atnos.eff._
import EvalEffect._
import Eff._
import cats.implicits._
import cats.{Applicative, Eval}
import org.scalameter.picklers.Implicits._

object EffBenchmark extends Bench.OfflineReport {
  type E = Fx.fx1[Eval]

  val sizes = Gen.enumeration("size")(10, 100, 1000, 40000)

  val vectors = for {
    size <- sizes
  } yield (0 until size).toVector

  def simpleSend[R, V](v: =>V)(implicit m: Member[Eval, R]) =
    delay(v)

  def traverseBackwards[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
    fa.foldLeft[G[Vector[B]]](G.pure(Vector.empty)){ (lglb, a) =>
      G.map2(lglb, f(a))(_ :+ _)
    }

  performance of "send" in {
    measure method "control" in {
      using(vectors) in { vector =>
        vector.traverse(Eval.later(_)).value
      }
    }
    measure method "simple send" in {
      using(vectors) in { vector =>
        run(runEval(vector.traverse(a => delay[E, Int](a))))
      }
    }
    measure method "backwards send" in {
      using(vectors) in { vector =>
        run(runEval(traverseBackwards(vector)(a => delay[E, Int](a))))
      }
    }
  }

}

