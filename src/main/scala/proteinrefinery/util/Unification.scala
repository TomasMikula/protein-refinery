package proteinrefinery.util

import nutcracker.Dom
import nutcracker.data.Promise
import nutcracker.data.Promise.{Completed, Conflict, Empty}

import scalaz.\&/.{Both, That, This}
import scalaz.{Equal, \&/}
import scalaz.Isomorphism.<=>
import scalaz.syntax.equal._

trait Unification[A] {
  type Update
  type Delta

  def unify(a1: A, a2: A): (Option[Delta], A, Option[Delta])
  //                            ^          ^      ^
  //                            |          |      |
  //                            |          |      +--- diff that takes `a2` to the unified value
  //                            |          +---------- the unified value
  //                            +--------------------- diff that takes `a1` to the unified value

  def dom: Dom.Aux[A, Update, Delta]

  def translate[B](iso: A <=> B): Unification.Aux[B, Update, Delta] =
    Unification.via[B, A](iso.flip)(this)
}

object Unification {
  type Aux[A, U, Δ] = Unification[A] { type Update = U; type Delta = Δ }

  def via[A, B](iso: A <=> B)(implicit UB: Unification[B]): Unification.Aux[A, UB.Update, UB.Delta] =
    new Unification[A] {
      type Update = UB.Update
      type Delta = UB.Delta

      def unify(a1: A, a2: A): (Option[Delta], A, Option[Delta]) = {
        val (d1, b, d2) = UB.unify(iso.to(a1), iso.to(a2))
        (d1, iso.from(b), d2)
      }

      def dom: Dom.Aux[A, Update, Delta] = Dom.via(iso)(UB.dom)
    }

  def tuple2[A, B](implicit UA: Unification[A], UB: Unification[B]): Unification.Aux[(A, B), UA.Update \&/ UB.Update, UA.Delta \&/ UB.Delta] =
    new Unification[(A, B)] {
      type Update = UA.Update \&/ UB.Update
      type Delta = UA.Delta \&/ UB.Delta

      def unify(ab1: (A, B), ab2: (A, B)): (Option[Delta], (A, B), Option[Delta]) = {
        val (da1, a, da2) = UA.unify(ab1._1, ab2._1)
        val (db1, b, db2) = UB.unify(ab1._2, ab2._2)
        (these(da1, db1), (a, b), these(da2, db2))
      }

      def dom: Dom.Aux[(A, B), Update, Delta] = Dom.tuple2(UA.dom, UB.dom)

      private def these[T, U](t: Option[T], u: Option[U]): Option[T \&/ U] = (t, u) match {
        case (Some(t), Some(u)) => Some(Both(t, u))
        case (Some(t), None   ) => Some(This(t))
        case (None   , Some(u)) => Some(That(u))
        case (None   , None   ) => None
      }
    }

  def promiseUnification[A: Equal]: Unification.Aux[Promise[A], Promise.Update[A], Promise.Delta[A]] = new Unification[Promise[A]] {
    type Update = Promise.Update[A]
    type Delta = Promise.Delta[A]

    def unify(p1: Promise[A], p2: Promise[A]): (Option[Delta], Promise[A], Option[Delta]) =
      (p1, p2) match {
        case (Completed(a1), Completed(a2)) => if (a1 === a2) (None, p1, None) else (Some(()), Conflict, Some(()))
        case (Conflict, Conflict) => (None, Conflict, None)
        case (Conflict, _) => (None, Conflict, Some(()))
        case (_, Conflict) => (Some(()), Conflict, None)
        case (Empty, Completed(a2)) => (Some(()), Completed(a2), None)
        case (Completed(a1), Empty) => (None, Completed(a1), Some(()))
        case (Empty, Empty) => (None, Empty, None)
      }

    def dom: Dom.Aux[Promise[A], Update, Delta] = Promise.promiseDomain[A]
  }

  trait Syntax {
    import scala.language.implicitConversions
    import Syntax._

    implicit def unificationOps[A](a: A)(implicit u: Unification[A]): UnificationOps[A, u.Update, u.Delta] =
      new UnificationOps[A, u.Update, u.Delta](a)(u)
  }

  object Syntax extends Syntax {
    class UnificationOps[A, U, Δ](a: A)(implicit u: Unification.Aux[A, U, Δ]) {
      def unify(b: A): (Option[Δ], A, Option[Δ]) = u.unify(a, b)
    }
  }
}