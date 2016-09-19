package proteinrefinery.util

import scala.annotation.tailrec
import scala.language.higherKinds

import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._

class AutoUnificationBag[M[_], A, Δ] private(
  elems: List[A],
  unify: (A, A) => M[Option[(Option[Δ], A, Option[Δ])]], // - unify(a, a) = Some(None, a, None)
                                                         // - symmetry
                                                         // - if update(a, u) = Some((b, δ)), then unify(a, b) =  Some(Some(δ), b, None)
                                                         // - if unify(a, b) = None, unify(a, c) = None, unify(b, c) = Some(_, d, _), then
                                                         //     unify(a, d) = None
  combineDeltas: (Δ, Δ) => Δ // must be associative, i.e. semigroup
) {
  def add(a: A)(implicit M: Monad[M]): M[(AutoUnificationBag[M, A, Δ], A, List[(A, Δ)])] =
    update(a).map(_.getOrElse((new AutoUnificationBag(a::elems, unify, combineDeltas), a, Nil)))

  def update(a: A)(implicit M: Monad[M]): M[Option[(AutoUnificationBag[M, A, Δ], A, List[(A, Δ)])]] = {
    //                     +---------------------------------------------- elems not touched by unification
    //                     |     +---------------------------------------- accumulation of the unified value
    //                     |     |      +--------------------------------- diff that takes `a` to the unified value
    //                     |     |      |        +------------------------ elems that were unified
    //                     |     |      |        |   +-------------------- the original element
    //                     |     |      |        |   |      +------------- delta to obtain the (current) unified value
    //                     |     |      |        |   |      |         +--- additional delta to combine (after) with deltas in the rest (tail)
    //                     |     |      |        |   |      |         |        of the list to obtain the final unified value
    //                     |     |      |        |   |      |         |
    //                     v     v      v        v   v      v         v
    elems.foldLeftM[M, (List[A], A, Option[Δ], List[(A, Option[Δ], Option[Δ])])]((Nil, a, None, Nil))((acc, elem) => {
      val (untouched, a, da, unified) = acc
      unify(a, elem) map {
        case Some((da1, a, de)) => (untouched, a, combineDeltasO(da, da1), (elem, de, da1) :: unified)
        case None => (elem::untouched, a, da, unified)
      }
    }) map { case (untouched, aa, da, unified0) =>

      @tailrec def applyDeltas(ads: List[(A, Option[Δ], Option[Δ])], d: Option[Δ], acc: List[(A, Option[Δ])]): List[(A, Option[Δ])] = ads match {
        case (a, dh, dt) :: tail => applyDeltas(tail, combineDeltasO(dt, d), (a, combineDeltasO(dh, d)) :: acc)
        case Nil => acc
      }

      val unified = applyDeltas(unified0, None, Nil)

      unified match {
        case Nil => None
        case (_, None) :: Nil => None // only one element was unified and its value has not changed
        case _ =>
          val unified1 = unified.map({ case (a, d) => (a, d.get) }) // more than one element were unified => all must have changed
          Some((new AutoUnificationBag(aa::untouched, unify, combineDeltas), aa, unified1))
      }
    }
  }

  private def combineDeltasO(d1: Option[Δ], d2: Option[Δ]): Option[Δ] = (d1, d2) match {
    case (Some(d1), Some(d2)) => Some(combineDeltas(d1, d2))
    case (None, d2) => d2
    case _ => d1
  }
}
