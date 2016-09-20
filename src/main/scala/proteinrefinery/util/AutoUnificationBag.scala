package proteinrefinery.util

import scala.annotation.tailrec
import scala.language.higherKinds

import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._

class AutoUnificationBag[M[_], A] private(private[util] val elems: List[A]) extends AnyVal {
  def add(a: A)(implicit M: Monad[M], U: Unification[M, A]): M[(AutoUnificationBag[M, A], A, List[(A, U.Delta)])] =
    update(a).map(_.getOrElse((new AutoUnificationBag(a::elems), a, Nil)))

  def update(a: A)(implicit M: Monad[M], U: Unification[M, A]): M[Option[(AutoUnificationBag[M, A], A, List[(A, U.Delta)])]] = {
    type Δ = U.Delta
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
      U.mustUnify(a, elem) map {
        case Some((da1, a, de)) => (untouched, a, combineDeltasO(da, da1)(U), (elem, de, da1) :: unified)
        case None => (elem::untouched, a, da, unified)
      }
    }) map { case (untouched, aa, da, unified0) =>

      @tailrec def applyDeltas(ads: List[(A, Option[Δ], Option[Δ])], d: Option[Δ], acc: List[(A, Option[Δ])]): List[(A, Option[Δ])] = ads match {
        case (a, dh, dt) :: tail => applyDeltas(tail, combineDeltasO(dt, d)(U), (a, combineDeltasO(dh, d)(U)) :: acc)
        case Nil => acc
      }

      val unified = applyDeltas(unified0, None, Nil)

      unified match {
        case Nil => None
        case (_, None) :: Nil => None // only one element was unified and its value has not changed
        case _ =>
          val unified1 = unified.map({ case (a, d) => (a, d.get) }) // more than one element were unified => all must have changed
          Some((new AutoUnificationBag(aa::untouched), aa, unified1))
      }
    }
  }

  private def combineDeltasO[Δ](d1: Option[Δ], d2: Option[Δ])(implicit U: Unification[M, A]{ type Delta = Δ }): Option[Δ] = (d1, d2) match {
    case (Some(d1), Some(d2)) => Some(U.dom.combineDeltas(d1, d2))
    case (None, d2) => d2
    case _ => d1
  }
}
