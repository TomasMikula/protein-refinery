package proteinrefinery.util

import nutcracker.Dom
import nutcracker.util.{DeepEqual, IsEqual}
import proteinrefinery.util.syntax._

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.Leibniz.===
import scalaz.std.list._
import scalaz.syntax.equal._
import scalaz.syntax.foldable._
import scalaz.{-\/, Equal, Foldable, Monoid, \/, \/-}

class AutoUnificationBag[A] private(private[util] val elems: List[A]) // extends AnyVal
{
  import AutoUnificationBag._

  def size: Int = elems.size

  def add[U, Δ](a: A)(implicit I: Identification.Aux[A, U, Δ]): (AutoUnificationBag[A], A, List[(A, Option[I.Delta])]) = {
    val (untouched, a1, deltas) = collect(a)
    (new AutoUnificationBag[A](a1 :: untouched.elems), a1, deltas)
  }

  def collect[U, Δ](a: A)(implicit I: Identification.Aux[A, U, Δ]): (AutoUnificationBag[A], A, List[(A, Option[I.Delta])]) = {
    type Δ = I.Delta

    //                 +---------------------------------------------- elems not touched by unification
    //                 |     +---------------------------------------- accumulation of the unified value
    //                 |     |      +--------------------------------- diff that takes `a` to the unified value
    //                 |     |      |        +------------------------ elems that were unified
    //                 |     |      |        |   +-------------------- the original element
    //                 |     |      |        |   |      +------------- delta to obtain the (current) unified value
    //                 |     |      |        |   |      |         +--- additional delta to combine (after) with deltas in the rest (tail)
    //                 |     |      |        |   |      |         |        of the list to obtain the final unified value
    //                 |     |      |        |   |      |         |
    //                 v     v      v        v   v      v         v
    elems.foldLeft[(List[A], A, Option[Δ], List[(A, Option[Δ], Option[Δ])])]((Nil, a, None, Nil))((acc, elem) => {
      val (untouched, a, da, unified) = acc
      I.unifyIfNecessary(a, elem) match {
        case Some((da1, a, de)) => (untouched, a, combineDeltasO(da, da1)(I), (elem, de, da1) :: unified)
        case None => (elem::untouched, a, da, unified)
      }
    }) match { case (untouched, aa, da, unified0) =>

      @tailrec def applyDeltas(ads: List[(A, Option[Δ], Option[Δ])], d: Option[Δ], acc: List[(A, Option[Δ])]): List[(A, Option[Δ])] = ads match {
        case (a, dh, dt) :: tail => applyDeltas(tail, combineDeltasO(dt, d)(I), (a, combineDeltasO(dh, d)(I)) :: acc)
        case Nil => acc
      }

      val unified = applyDeltas(unified0, None, Nil)

      (new AutoUnificationBag[A](untouched), aa, unified)
    }
  }

  def union[U, Δ](that: AutoUnificationBag[A])(implicit I: Identification.Aux[A, U, Δ]): AutoUnificationBag[A] = {
    val (bag, elems) = if (this.size >= that.size) (this, that.elems) else (that, this.elems)
    bag.addAll(elems)
  }

  def union1[U, Δ](that: AutoUnificationBag[A])(implicit
    I: Identification.Aux[A, U, Δ],
    A: Equal[A]
  ): (Delta[A, I.Delta], AutoUnificationBag[A], Delta[A, I.Delta]) = {
    // XXX doing addAll1 twice. Could possibly be optimized
    val (thisBag, thisDelta) = this addAll1 that.elems
    val (thatBag, thatDelta) = that addAll1 this.elems
    assert(thisBag === thatBag)
    (thisDelta, thisBag, thatDelta)
  }

  def addAll[F[_], U, Δ](fa: F[A])(implicit I: Identification.Aux[A, U, Δ], F: Foldable[F]): AutoUnificationBag[A] =
    fa.foldLeft(this)((bag, elem) => bag.add(elem)._1)

  def addAll1[F[_], U, Δ](fa: F[A])(implicit
    I: Identification.Aux[A, U, Δ],
    F: Foldable[F],
    A: Equal[A]
  ): (AutoUnificationBag[A], Delta[A, I.Delta]) = {
    type Δ = I.Delta

    fa.foldLeft[(AutoUnificationBag[A], Delta[A, Δ])]((this, Delta.empty))((acc, elem) => {
      val (bag, delta) = acc
      bag.add(elem) match { case (bag, addedElem, deltas) =>
        (bag, delta.append(addedElem, deltas))
      }
    })
  }

  def list: List[A] =
    elems

  def foreach(f: A => Unit): Unit =
    elems.foreach(f)

  def map[U, Δ, B](f: A => B)(implicit I: Identification.Aux[B, U, Δ]): AutoUnificationBag[B] =
    AutoUnificationBag.empty[B].addAll(elems.map(f))

  /** Like `map`, but assumes that `f` preserves the non-obligation to unify. In other words,
    * this method assumes that if `a1`, `a2` do not have to be unified, then `f(a1)`, `f(a2)`
    * do not have to be unified.
    */
  def inject[B](f: A => B): AutoUnificationBag[B] =
    new AutoUnificationBag(elems.map(f))

  /** When `A =:= (K, V)`, this bag can be viewed as a map. This method assumes that
    * the identification on `K` (as later used with the map) doesn't have to unify `k1` with `k2` if the
    * identification on `(K, V)` doesn't have to unify `(k1, v1)` with `(k2, v2)` for any `v1`, `v2`.
    */
  def asMap[K, V](implicit ev: A === (K, V)): AutoUnificationMap[K, V] =
    new AutoUnificationMap[K, V](ev.subst[List](elems))

  /** This method assumes that the identification on `K` (as later used with the map)
    * doesn't have to unify `f(a1)` with `f(a2)` if the identification on `A` doesn't
    * have to unify `a1` with `a2`.
    */
  def toMap[K, V](f: A => (K, V)): AutoUnificationMap[K, V] =
    new AutoUnificationMap[K, V](elems.map(f))

  private def combineDeltasO[Δ](d1: Option[Δ], d2: Option[Δ])(implicit I: Identification[A]{ type Delta = Δ }): Option[Δ] = (d1, d2) match {
    case (Some(d1), Some(d2)) => Some(I.unification.dom.combineDeltas(d1, d2))
    case (None, d2) => d2
    case _ => d1
  }

  override def toString: String = elems.toString()
  override def equals(other: Any): Boolean =
    if(other.isInstanceOf[AutoUnificationBag[A]]) {
      val that = other.asInstanceOf[AutoUnificationBag[A]]
      (this.size == that.size) && (this.elems.forall(that.elems.contains(_))) // XXX O(n^2)
    } else {
      false
    }
}

object AutoUnificationBag {

  case class Delta[A, Δ] private(private val roots: List[Delta.Node[A, Δ]]) {
    import Delta._

    private val flat: Need1[Dom[A] { type Delta = Δ }, (List[A], List[(A, Δ)])] =
      Need1(dom => flatten(dom))

    def isEmpty[U](implicit dom: Dom.Aux[A, U, Δ]): Boolean =
      newElements.isEmpty && updatedElements.isEmpty

    def ifNonEmpty[U](implicit dom: Dom.Aux[A, U, Δ]): Option[Delta[A, Δ]] =
      if(isEmpty) None
      else Some(this)

    def append(addedElem: A, updatedElems: List[(A, Option[Δ])])(implicit A: Equal[A]): Delta[A, Δ] =
      updatedElems.foldLeft[(List[Node[A, Δ]], List[(PreExisting[A] \/ Node[A, Δ], Option[Δ])])]((roots, Nil))({ case ((roots, addedElemChildren), (updatedElem, delta)) =>
        roots.removeFirst(_.value === updatedElem) match {
          case Some((node, roots)) => (roots, (\/-(node), delta) :: addedElemChildren)
          case None => (roots, (-\/(PreExisting[A](updatedElem)), delta) :: addedElemChildren)
        }
      }) match { case (roots, addedElemChildren) => new Delta(Node(addedElem, addedElemChildren) :: roots) }

    def append[U](that: Delta[A, Δ])(implicit dom: Dom.Aux[A, U, Δ], A: Equal[A]): Delta[A, Δ] =
      that.roots.foldLeft(this)(_ append _)

    def newElements[U](implicit dom: Dom.Aux[A, U, Δ]): List[A] =
      flat()._1

    def updatedElements[U](implicit dom: Dom.Aux[A, U, Δ]): List[(A, Δ)] =
      flat()._2

    private def append[U](node: Node[A, Δ])(implicit dom: Dom.Aux[A, U, Δ], A: Equal[A]): Delta[A, Δ] = {
      val (elem, updatedElems) = node.flatten
      append(elem, updatedElems)
    }

    private def flatten[U](implicit dom: Dom.Aux[A, U, Δ]): (List[A], List[(A, Δ)]) = {
      val (newElems, modifiedElems) = roots.map(_.flatten).split({
        case (a, Nil) => Left(a)
        case (_, d::ds) => Right((d, ds))
      })
      (newElems, modifiedElems.flatMap(dds => {
        val (d, ds) = dds
        ds match {
          case Nil =>
            val (a, dOpt) = d
            dOpt match {
              case Some(δ) => List((a, δ))
              case None => List() // only one element unified with element from fa, but unchanged, thus don't report
            }
          case ds =>
            (d::ds).map(ad => (ad._1, ad._2.get)) // if there are multiple elements, no delta can be None
        }
      }))
    }
  }

  object Delta {

    private[Delta] case class Node[A, Δ](value: A, children: List[(PreExisting[A] \/ Node[A, Δ], Option[Δ])]) {
      def flatten[U](implicit dom: Dom.Aux[A, U, Δ]): (A, List[(A, Option[Δ])]) =
        (value, children.flatMap({ case (child, delta) => child match {
          case -\/(PreExisting(a)) => List((a, delta))
          case \/-(node) =>
            node.flatten._2.map({ case (a, d) => (a, combineDeltasOpt(d, delta)) })
        }}))
    }
    private[Delta] case class PreExisting[A](value: A)

    def empty[A, Δ] = new Delta[A, Δ](Nil)

    private def combineDeltasOpt[A, U, Δ](d1: Option[Δ], d2: Option[Δ])(implicit dom: Dom.Aux[A, U, Δ]): Option[Δ] = (d1, d2) match {
      case (Some(d1), Some(d2)) => Some(dom.combineDeltas(d1, d2))
      case (None, d2) => d2
      case _ => d1
    }

  }

  def empty[A]: AutoUnificationBag[A] = new AutoUnificationBag(Nil)

  def apply[U, Δ, A](as: A*)(implicit I: Identification.Aux[A, U, Δ]): AutoUnificationBag[A] =
    empty[A].addAll(as)

  implicit def equalInstance[A: Equal]: Equal[AutoUnificationBag[A]] = new Equal[AutoUnificationBag[A]] {
    def equal(bag1: AutoUnificationBag[A], bag2: AutoUnificationBag[A]): Boolean =
      (bag1.size == bag2.size) && (bag1.elems.forall(a1 => bag2.elems.exists(a2 => a1 === a2))) // XXX O(n^2) // XXX not correct wrt. number of occurrences
  }

  implicit def deepEqualInstance[Ptr1[_], Ptr2[_], A, B](implicit ev: DeepEqual[A, B, Ptr1, Ptr2]): DeepEqual[AutoUnificationBag[A], AutoUnificationBag[B], Ptr1, Ptr2] =
    new DeepEqual[AutoUnificationBag[A], AutoUnificationBag[B], Ptr1, Ptr2] {
      def equal(a1: AutoUnificationBag[A], a2: AutoUnificationBag[B]): IsEqual[Ptr1, Ptr2] =
        IsEqual.unorderedListEqual(a1.list, a2.list)
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