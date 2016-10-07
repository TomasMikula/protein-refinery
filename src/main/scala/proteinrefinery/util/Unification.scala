package proteinrefinery.util

import nutcracker.Promise.{Completed, Conflict, Empty}

import scala.language.higherKinds
import nutcracker.{Dom, Promise}

import scalaz.\&/.{Both, That, This}
import scalaz.{Applicative, Equal, Functor, Monad, MonadPartialOrder, \&/}
import scalaz.Isomorphism.<=>
import scalaz.syntax.equal._
import scalaz.syntax.monad._

trait Unification[A] {
  type Update
  type Delta
  type F[_]

  // Laws:
  //  - mustUnify(a, b) = mustUnify(b, a)
  //  - if update_(a, u) = a' and mustUnify(a, b), then mustUnify(a', b)
  //  - if mustUnify(a, unify(b, c)), then mustUnify(a, b) or mustUnify(a, c)
  // Additionally, for reflexive identification:
  //  - mustUnify(a, a)
  def mustUnify(a1: A, a2: A): Boolean


  //                                                                    +----------------------------------- an effect to track inevitable failure (obligation to unify and
  //                                                                    |                                        impossibility to unify at the same time)
  //                                                                    |    +------------------------------ None means no obligation to unify, Some means obligation to unify
  //                                                                    |    |        +--------------------- diff that takes `a1` to the unified value
  //                                                                    |    |        |          +---------- the unified value
  //                                                                    |    |        |          |      +--- diff that takes `a2` to the unified value
  //                                                                    |    |        |          |      |
  //                                                                    v    v        v          v      v
  final def unifyIfNecessary(a1: A, a2: A)(implicit F: Applicative[F]): F[Option[(Option[Delta], A, Option[Delta])]] =
    if(mustUnify(a1, a2)) unify(a1, a2).map(Some(_))
    else F.point(None)

  def unify(a1: A, a2: A): F[(Option[Delta], A, Option[Delta])]
  //                       ^      ^          ^      ^
  //                       |      |          |      |
  //                       |      |          |      +--- diff that takes `a2` to the unified value
  //                       |      |          +---------- the unified value
  //                       |      +--------------------- diff that takes `a1` to the unified value
  //                       +---------------------------- an effect to track inevitable failure (obligation to unify and
  //                                                         impossibility to unify at the same time)

  def dom: Dom.Aux[A, Update, Delta]

  def promote[N[_]](implicit mn: MonadPartialOrder[N, F]): Unification.Aux[A, Update, Delta, N] = new Unification[A] {
    type Update = Unification.this.Update
    type Delta = Unification.this.Delta
    type F[X] = N[X]

    def mustUnify(a1: A, a2: A): Boolean =
      Unification.this.mustUnify(a1, a2)

    def unify(a1: A, a2: A): N[(Option[Delta], A, Option[Delta])] =
      mn(Unification.this.unify(a1, a2))

    def dom: Dom.Aux[A, Update, Delta] = Unification.this.dom
  }
}

object Unification {
  type Aux0[A, M[_]] = Unification[A] { type F[X] = M[X] }
  type Aux[A, U, Δ, M[_]] = Unification[A] { type Update = U; type Delta = Δ; type F[X] = M[X] }

  def via[M[_], A, B](iso: A <=> B)(implicit UB: Unification.Aux0[B, M], M: Functor[M]): Unification.Aux[A, UB.Update, UB.Delta, M] =
    new Unification[A] {
      type Update = UB.Update
      type Delta = UB.Delta
      type F[X] = M[X]

      def mustUnify(a1: A, a2: A): Boolean =
        UB.mustUnify(iso.to(a1), iso.to(a2))

      def unify(a1: A, a2: A): M[(Option[Delta], A, Option[Delta])] =
        UB.unify(iso.to(a1), iso.to(a2)).map({ case (d1, b, d2) => (d1, iso.from(b), d2) })

      def dom: Dom.Aux[A, Update, Delta] = Dom.via(iso)(UB.dom)
    }

  def tuple2[M[_], A, B](implicit UA: Unification.Aux0[A, M], UB: Unification.Aux0[B, M], M: Monad[M]): Unification.Aux[(A, B), UA.Update \&/ UB.Update, UA.Delta \&/ UB.Delta, M] =
    new Unification[(A, B)] {
      type Update = UA.Update \&/ UB.Update
      type Delta = UA.Delta \&/ UB.Delta
      type F[X] = M[X]

      def mustUnify(ab1: (A, B), ab2: (A, B)): Boolean =
        UA.mustUnify(ab1._1, ab2._1) || UB.mustUnify(ab1._2, ab2._2)

      def unify(ab1: (A, B), ab2: (A, B)): F[(Option[Delta], (A, B), Option[Delta])] =
        M.apply2(UA.unify(ab1._1, ab2._1), UB.unify(ab1._2, ab2._2))((dad, dbd) => {
          val (da1, a, da2) = dad
          val (db1, b, db2) = dbd
          (these(da1, db1), (a, b), these(da2, db2))
        })

      def dom: Dom.Aux[(A, B), Update, Delta] = Dom.tuple2(UA.dom, UB.dom)

      private def these[T, U](t: Option[T], u: Option[U]): Option[T \&/ U] = (t, u) match {
        case (Some(t), Some(u)) => Some(Both(t, u))
        case (Some(t), None   ) => Some(This(t))
        case (None   , Some(u)) => Some(That(u))
        case (None   , None   ) => None
      }
    }

  def obligatoryPromiseUnification[A: Equal]: Unification.Aux0[Promise[A], Option] = new Unification[Promise[A]] {
    type Update = Promise.Update[A]
    type Delta = Promise.Delta[A]
    type F[X] = Option[X]

    def mustUnify(p1: Promise[A], p2: Promise[A]): Boolean =
      (p1, p2) match {
        case (Completed(a1), Completed(a2)) => a1 === a2
        case (Conflict, _) => true
        case (_, Conflict) => true
        case _ => false
      }

    def unify(p1: Promise[A], p2: Promise[A]): Option[(Option[Delta], Promise[A], Option[Delta])] =
      canUnifyPromise(p1, p2)

    def dom: Dom.Aux[Promise[A], Update, Delta] = Promise.promiseDomain[A]
  }

  def optionalPromiseUnification[A: Equal]: Unification.Aux0[Promise[A], Option] = new Unification[Promise[A]] {
    type Update = Promise.Update[A]
    type Delta = Promise.Delta[A]
    type F[X] = Option[X]

    def mustUnify(p1: Promise[A], p2: Promise[A]): Boolean =
      false

    def unify(p1: Promise[A], p2: Promise[A]): Option[(Option[Delta], Promise[A], Option[Delta])] =
      canUnifyPromise(p1, p2)

    def dom: Dom.Aux[Promise[A], Update, Delta] = Promise.promiseDomain[A]
  }

  private def canUnifyPromise[A: Equal](p1: Promise[A], p2: Promise[A]): Option[(Option[Promise.Delta[A]], Promise[A], Option[Promise.Delta[A]])] =
    (p1, p2) match {
      case (Completed(a1), Completed(a2)) => if (a1 === a2) Some((None, p1, None)) else None
      case (Conflict, _) => None // pretend failure to unify, instead of unifying with a failure (which is always possible)
      case (_, Conflict) => None // pretend failure to unify, instead of unifying with a failure (which is always possible)
      case (Empty, Completed(a2)) => Some((Some(()), Completed(a2), None))
      case (Completed(a1), Empty) => Some((None, Completed(a1), Some(())))
      case (Empty, Empty) => Some((None, Empty, None))
    }
}