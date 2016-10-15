package proteinrefinery.util

import scala.language.higherKinds

import nutcracker.Promise
import nutcracker.Promise.{Completed, Conflict}

import scalaz.{Applicative, Equal, Functor, Monad, MonadPartialOrder, \&/}
import scalaz.Id.Id
import scalaz.Isomorphism.<=>
import scalaz.syntax.equal._
import scalaz.syntax.functor._

trait Identification[A] {
  type Update
  type Delta
  type F[_]

  // Laws:
  //  - necessarilySame(a, b) = necessarilySame(b, a)
  //  - if update_(a, u) = a' and necessarilySame(a, b), then necessarilySame(a', b)
  //  - if necessarilySame(a, unify(b, c)), then necessarilySame(a, b) or necessarilySame(a, c)
  def necessarilySame(a1: A, a2: A): Boolean

  def unification: Unification.Aux[A, Update, Delta, F]

  //                                                                    +----------------------------------- an effect to track inevitable failure (obligation to unify and
  //                                                                    |                                        impossibility to unify at the same time)
  //                                                                    |    +------------------------------ None means no obligation to unify, Some means obligation to unify
  //                                                                    |    |        +--------------------- diff that takes `a1` to the unified value
  //                                                                    |    |        |          +---------- the unified value
  //                                                                    |    |        |          |      +--- diff that takes `a2` to the unified value
  //                                                                    |    |        |          |      |
  //                                                                    v    v        v          v      v
  final def unifyIfNecessary(a1: A, a2: A)(implicit F: Applicative[F]): F[Option[(Option[Delta], A, Option[Delta])]] =
    if(necessarilySame(a1, a2)) unification.unify(a1, a2).map(Some(_))
    else F.point(None)

  def promote[N[_]](implicit mn: MonadPartialOrder[N, F]): Identification.Aux[A, Update, Delta, N] = new Identification[A] {
    type Update = Identification.this.Update
    type Delta = Identification.this.Delta
    type F[X] = N[X]

    def necessarilySame(a1: A, a2: A): Boolean = Identification.this.necessarilySame(a1, a2)

    def unification: Unification.Aux[A, Update, Delta, N] =
      Identification.this.unification.promote[N]
  }

  def translate[B](iso: A <=> B)(implicit F: Functor[F]): Identification.Aux[B, Update, Delta, F] =
    Identification.via[F, B, A](iso.flip)(this, F)

  def zoomOut[B](f: B => A)(implicit UB: Unification[B]): Identification.Aux[B, UB.Update, UB.Delta, UB.F] =
    Identification.by[B, A](f)(UB, this)
}

object Identification {
  type Aux0[A, M[_]] = Identification[A] { type F[X] = M[X] }
  type Aux[A, U, Δ, M[_]] = Identification[A] { type Update = U; type Delta = Δ; type F[X] = M[X] }

  def via[M[_], A, B](iso: A <=> B)(implicit IB: Identification.Aux0[B, M], M: Functor[M]): Identification.Aux[A, IB.Update, IB.Delta, M] =
    new Identification[A] {
      type Update = IB.Update
      type Delta = IB.Delta
      type F[X] = M[X]

      def necessarilySame(a1: A, a2: A): Boolean =
        IB.necessarilySame(iso.to(a1), iso.to(a2))

      def unification: Unification.Aux[A, IB.Update, IB.Delta, M] = Unification.via[M, A, B](iso)(IB.unification, M)
    }

  def by[A, B](f: A => B)(implicit U: Unification[A], IB: Identification[B]): Identification.Aux[A, U.Update, U.Delta, U.F] =
    new Identification[A] {
      type Update = U.Update
      type Delta = U.Delta
      type F[X] = U.F[X]

      def necessarilySame(a1: A, a2: A): Boolean =
        IB.necessarilySame(f(a1), f(a2))

      def unification: Unification.Aux[A, Update, Delta, F] = U
    }

  def tuple2[M[_], A, B](implicit IA: Identification.Aux0[A, M], IB: Identification.Aux0[B, M], M: Monad[M]): Identification.Aux[(A, B), IA.Update \&/ IB.Update, IA.Delta \&/ IB.Delta, M] =
    new Identification[(A, B)] {
      type Update = IA.Update \&/ IB.Update
      type Delta = IA.Delta \&/ IB.Delta
      type F[X] = M[X]

      def necessarilySame(ab1: (A, B), ab2: (A, B)): Boolean =
        IA.necessarilySame(ab1._1, ab2._1) || IB.necessarilySame(ab1._2, ab2._2)

      def unification: Unification.Aux[(A, B), IA.Update \&/ IB.Update, IA.Delta \&/ IB.Delta, M] =
        Unification.tuple2[M, A, B](IA.unification, IB.unification, M)
    }

  def promiseIdentification[A: Equal]: Identification.Aux[Promise[A], Promise.Update[A], Promise.Delta[A], Id] =
    new Identification[Promise[A]] {
      type Update = Promise.Update[A]
      type Delta = Promise.Delta[A]
      type F[X] = Id[X]

      def necessarilySame(p1: Promise[A], p2: Promise[A]): Boolean =
        (p1, p2) match {
          case (Completed(a1), Completed(a2)) => a1 === a2
          case (Conflict, _) => true
          case (_, Conflict) => true
          case _ => false
        }

      def unification: Unification.Aux[Promise[A], Update, Delta, Id] =
        Unification.promiseUnification[A]
    }
}