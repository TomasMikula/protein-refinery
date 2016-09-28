package proteinrefinery.util

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.{Foldable, Monad, Monoid}
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._

class AutoUnificationBag[M[_], A] private(private[util] val elems: List[A]) extends AnyVal {
  def size: Int = elems.size

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

  def union(that: AutoUnificationBag[M, A])(implicit M: Monad[M], U: Unification[M, A]): M[AutoUnificationBag[M, A]] = {
    val (bag, elems) = if (this.size >= that.size) (this, that.elems) else (that, this.elems)
    bag.addAll(elems)
  }

  def addAll[F[_]](fa: F[A])(implicit M: Monad[M], U: Unification[M, A], F: Foldable[F]): M[AutoUnificationBag[M, A]] =
    fa.foldLeftM(this)((bag, elem) => bag.add(elem).map(_._1))

  def list: List[A] =
    elems

  def foreach(f: A => Unit): Unit =
    elems.foreach(f)

  def map[N[_], B](f: A => B)(implicit N: Monad[N], U: Unification[N, B]): N[AutoUnificationBag[N, B]] =
    AutoUnificationBag.empty[N, B].addAll(elems.map(f))

  /** Like `map`, but assumes that `f` preserves the non-obligation to unify. In other words,
    * this method assumes that if `a1`, `a2` do not have to be unified, then `f(a1)`, `f(a2)`
    * do not have to be unified.
    */
  def inject[B](f: A => B): AutoUnificationBag[M, B] =
    new AutoUnificationBag(elems.map(f))

  private def combineDeltasO[Δ](d1: Option[Δ], d2: Option[Δ])(implicit U: Unification[M, A]{ type Delta = Δ }): Option[Δ] = (d1, d2) match {
    case (Some(d1), Some(d2)) => Some(U.dom.combineDeltas(d1, d2))
    case (None, d2) => d2
    case _ => d1
  }
}

object AutoUnificationBag {
  def empty[M[_], A]: AutoUnificationBag[M, A] = new AutoUnificationBag(Nil)

  def apply[M[_], A](as: A*)(implicit M: Monad[M], U: Unification[M, A]): M[AutoUnificationBag[M, A]] =
    empty[M, A].addAll(as)

  implicit def foldableInstance[M[_]]: Foldable[AutoUnificationBag[M, ?]] = new Foldable[AutoUnificationBag[M, ?]] {

    def foldMap[A, B](fa: AutoUnificationBag[M, A])(f: (A) => B)(implicit B: Monoid[B]): B =
      Foldable[List].foldMap(fa.elems)(f)

    def foldRight[A, B](fa: AutoUnificationBag[M, A], z: => B)(f: (A, => B) => B): B =
      Foldable[List].foldRight(fa.elems, z)(f)
  }

  private implicit val foldableSeq: Foldable[Seq] = new Foldable[Seq] {
    def foldMap[A, B](fa: Seq[A])(f: (A) => B)(implicit B: Monoid[B]): B =
      fa.foldLeft(B.zero)((b, a) => B.append(b, f(a)))

    def foldRight[A, B](fa: Seq[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
  }
}