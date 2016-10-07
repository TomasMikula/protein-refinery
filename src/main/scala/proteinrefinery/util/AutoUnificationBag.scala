package proteinrefinery.util

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.Leibniz.===
import scalaz.{Equal, Foldable, Monad, Monoid}
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.equal._
import scalaz.syntax.foldable._

class AutoUnificationBag[A] private(private[util] val elems: List[A]) extends AnyVal {
  def size: Int = elems.size

  def add[M[_]](a: A)(implicit I: Identification.Aux0[A, M], M: Monad[M]): M[(AutoUnificationBag[A], A, List[(A, Option[I.Delta])])] = {
    collect(a).map({ case (untouched, a, deltas) => (new AutoUnificationBag[A](a :: untouched.elems), a, deltas) })
  }

  def collect[M[_]](a: A)(implicit I: Identification.Aux0[A, M], M: Monad[M]): M[(AutoUnificationBag[A], A, List[(A, Option[I.Delta])])] = {
    type Δ = I.Delta

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
      I.unifyIfNecessary(a, elem) map {
        case Some((da1, a, de)) => (untouched, a, combineDeltasO(da, da1)(I), (elem, de, da1) :: unified)
        case None => (elem::untouched, a, da, unified)
      }
    }) map { case (untouched, aa, da, unified0) =>

      @tailrec def applyDeltas(ads: List[(A, Option[Δ], Option[Δ])], d: Option[Δ], acc: List[(A, Option[Δ])]): List[(A, Option[Δ])] = ads match {
        case (a, dh, dt) :: tail => applyDeltas(tail, combineDeltasO(dt, d)(I), (a, combineDeltasO(dh, d)(I)) :: acc)
        case Nil => acc
      }

      val unified = applyDeltas(unified0, None, Nil)

      (new AutoUnificationBag[A](untouched), aa, unified)
    }
  }

  def union[M[_]](that: AutoUnificationBag[A])(implicit I: Identification.Aux0[A, M], M: Monad[M]): M[AutoUnificationBag[A]] = {
    val (bag, elems) = if (this.size >= that.size) (this, that.elems) else (that, this.elems)
    bag.addAll(elems)
  }

  def addAll[F[_], M[_]](fa: F[A])(implicit I: Identification.Aux0[A, M], M: Monad[M], F: Foldable[F]): M[AutoUnificationBag[A]] =
    fa.foldLeftM(this)((bag, elem) => bag.add(elem).map(_._1))

  def list: List[A] =
    elems

  def foreach(f: A => Unit): Unit =
    elems.foreach(f)

  def map[N[_], B](f: A => B)(implicit I: Identification.Aux0[B, N], N: Monad[N]): N[AutoUnificationBag[B]] =
    AutoUnificationBag.empty[B].addAll(elems.map(f))

  /** Like `map`, but assumes that `f` preserves the non-obligation to unify. In other words,
    * this method assumes that if `a1`, `a2` do not have to be unified, then `f(a1)`, `f(a2)`
    * do not have to be unified.
    */
  def inject[B](f: A => B): AutoUnificationBag[B] =
    new AutoUnificationBag(elems.map(f))

  /** For when `A =:= (K, V)`, this bag can be transformed to a map. This method assumes that
    * the unification on `K` (as later used with the map) doesn't have to unify `k1` with `k2` if the
    * unification on `(K, V)` doesn't have to unify `(k1, v1)` with `(k2, v2)` for any `v1`, `v2`.
    */
  def restrictToMap[K, V](implicit ev: A === (K, V)): AutoUnificationMap[K, V] =
    new AutoUnificationMap[K, V](ev.subst[List](elems))

  private def combineDeltasO[Δ](d1: Option[Δ], d2: Option[Δ])(implicit I: Identification[A]{ type Delta = Δ }): Option[Δ] = (d1, d2) match {
    case (Some(d1), Some(d2)) => Some(I.unification.dom.combineDeltas(d1, d2))
    case (None, d2) => d2
    case _ => d1
  }

  override def toString: String = elems.toString()
}

object AutoUnificationBag {
  def empty[A]: AutoUnificationBag[A] = new AutoUnificationBag(Nil)

  def apply[M[_], A](as: A*)(implicit I: Identification.Aux0[A, M], M: Monad[M]): M[AutoUnificationBag[A]] =
    empty[A].addAll(as)

  implicit def equalInstance[A: Equal]: Equal[AutoUnificationBag[A]] = new Equal[AutoUnificationBag[A]] {
    def equal(bag1: AutoUnificationBag[A], bag2: AutoUnificationBag[A]): Boolean =
      (bag1.size == bag2.size) && (bag1.elems.forall(a1 => bag2.elems.exists(a2 => a1 === a2)))
  }

  implicit def foldableInstance: Foldable[AutoUnificationBag] = new Foldable[AutoUnificationBag] {

    def foldMap[A, B](fa: AutoUnificationBag[A])(f: (A) => B)(implicit B: Monoid[B]): B =
      Foldable[List].foldMap(fa.elems)(f)

    def foldRight[A, B](fa: AutoUnificationBag[A], z: => B)(f: (A, => B) => B): B =
      Foldable[List].foldRight(fa.elems, z)(f)
  }

  private implicit val foldableSeq: Foldable[Seq] = new Foldable[Seq] {
    def foldMap[A, B](fa: Seq[A])(f: (A) => B)(implicit B: Monoid[B]): B =
      fa.foldLeft(B.zero)((b, a) => B.append(b, f(a)))

    def foldRight[A, B](fa: Seq[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
  }
}