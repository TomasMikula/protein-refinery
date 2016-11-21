package proteinrefinery.util

import nutcracker.Promise.{Completed, Conflict}
import nutcracker.{Dom, Promise}

import scalaz.Isomorphism.<=>
import scalaz.syntax.equal._
import scalaz.{Equal, \&/}

trait Identification[A] {
  type Update
  type Delta

  // Laws:
  //  - necessarilySame(a, b) = necessarilySame(b, a)
  //  - if update_(a, u) = a' and necessarilySame(a, b), then necessarilySame(a', b)
  //  - if necessarilySame(a, unify(b, c)), then necessarilySame(a, b) or necessarilySame(a, c)
  def necessarilySame(a1: A, a2: A): Boolean

  def unification: Unification.Aux[A, Update, Delta]

  def dom: Dom.Aux[A, Update, Delta] = unification.dom

  //                                           +------------------------------ None means no obligation to unify, Some means obligation to unify
  //                                           |        +--------------------- diff that takes `a1` to the unified value
  //                                           |        |          +---------- the unified value
  //                                           |        |          |      +--- diff that takes `a2` to the unified value
  //                                           |        |          |      |
  //                                           v        v          v      v
  final def unifyIfNecessary(a1: A, a2: A): Option[(Option[Delta], A, Option[Delta])] =
    if(necessarilySame(a1, a2)) Some(unification.unify(a1, a2))
    else None

  def translate[B](iso: A <=> B): Identification.Aux[B, Update, Delta] =
    Identification.via[B, A](iso.flip)(this)

  def zoomOut[B](f: B => A)(implicit UB: Unification[B]): Identification.Aux[B, UB.Update, UB.Delta] =
    Identification.by[B, A](f)(UB, this)
}

object Identification {
  type Aux[A, U, Δ] = Identification[A] { type Update = U; type Delta = Δ }

  def via[A, B](iso: A <=> B)(implicit IB: Identification[B]): Identification.Aux[A, IB.Update, IB.Delta] =
    new Identification[A] {
      type Update = IB.Update
      type Delta = IB.Delta

      def necessarilySame(a1: A, a2: A): Boolean =
        IB.necessarilySame(iso.to(a1), iso.to(a2))

      def unification: Unification.Aux[A, IB.Update, IB.Delta] = Unification.via[A, B](iso)(IB.unification)
    }

  def by[A, B](f: A => B)(implicit U: Unification[A], IB: Identification[B]): Identification.Aux[A, U.Update, U.Delta] =
    new Identification[A] {
      type Update = U.Update
      type Delta = U.Delta

      def necessarilySame(a1: A, a2: A): Boolean =
        IB.necessarilySame(f(a1), f(a2))

      def unification: Unification.Aux[A, Update, Delta] = U
    }

  def tuple2[A, B](implicit IA: Identification[A], IB: Identification[B]): Identification.Aux[(A, B), IA.Update \&/ IB.Update, IA.Delta \&/ IB.Delta] =
    new Identification[(A, B)] {
      type Update = IA.Update \&/ IB.Update
      type Delta = IA.Delta \&/ IB.Delta

      def necessarilySame(ab1: (A, B), ab2: (A, B)): Boolean =
        IA.necessarilySame(ab1._1, ab2._1) || IB.necessarilySame(ab1._2, ab2._2)

      def unification: Unification.Aux[(A, B), IA.Update \&/ IB.Update, IA.Delta \&/ IB.Delta] =
        Unification.tuple2[A, B](IA.unification, IB.unification)
    }

  def promiseIdentification[A: Equal]: Identification.Aux[Promise[A], Promise.Update[A], Promise.Delta[A]] =
    new Identification[Promise[A]] {
      type Update = Promise.Update[A]
      type Delta = Promise.Delta[A]

      def necessarilySame(p1: Promise[A], p2: Promise[A]): Boolean =
        (p1, p2) match {
          case (Completed(a1), Completed(a2)) => a1 === a2
          case (Conflict, _) => true
          case (_, Conflict) => true
          case _ => false
        }

      def unification: Unification.Aux[Promise[A], Update, Delta] =
        Unification.promiseUnification[A]
    }

  trait Syntax {
    import scala.language.implicitConversions
    import Syntax._

    implicit def identificationOps[A](a: A)(implicit i: Identification[A]): IdentificationOps[A, i.Update, i.Delta] =
      new IdentificationOps[A, i.Update, i.Delta](a)(i)
  }

  object Syntax extends Syntax {
    class IdentificationOps[A, U, Δ](a: A)(implicit i: Identification.Aux[A, U, Δ]) {
      def unifyIfNecessary(b: A): Option[(Option[Δ], A, Option[Δ])] = i.unifyIfNecessary(a, b)
      def necessarilySame(b: A): Boolean = i.necessarilySame(a, b)
    }
  }
}