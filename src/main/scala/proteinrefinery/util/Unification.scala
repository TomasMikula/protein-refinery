package proteinrefinery.util

import scala.language.higherKinds
import nutcracker.Dom

import scalaz.\&/

trait Unification[M[_], A] {
  type Update
  type Delta

  // - unify(a, a) = Some(None, a, None)
  // - symmetry
  // - if update(a, u) = Some((b, δ)), then unify(a, b) =  Some(Some(δ), b, None)
  // - if unify(a, b) = None, unify(a, c) = None, unify(b, c) = Some(_, d, _), then
  //     unify(a, d) = None
  def mustUnify(a1: A, a2: A): M[Option[(Option[Delta], A, Option[Delta])]]
  //                           ^    ^        ^          ^      ^
  //                           |    |        |          |      |
  //                           |    |        |          |      +--- diff that takes `a2` to the unified value
  //                           |    |        |          +---------- the unified value
  //                           |    |        +--------------------- diff that takes `a1` to the unified value
  //                           |    +------------------------------ None means no obligation to unify, Some means obligation to unify
  //                           +----------------------------------- an effect to track inevitable failure (obligation to unify and
  //                                                                    impossibility to unify at the same time)

  def canUnify(a1: A, a2: A): M[(Option[Delta], A, Option[Delta])]
  //                          ^      ^          ^      ^
  //                          |      |          |      |
  //                          |      |          |      +--- diff that takes `a2` to the unified value
  //                          |      |          +---------- the unified value
  //                          |      +--------------------- diff that takes `a1` to the unified value
  //                          +---------------------------- an effect to track inevitable failure (obligation to unify and
  //                                                            impossibility to unify at the same time)

  def dom: Dom.Aux[A, Update, Delta]
}

object Unification {
  type Aux[M[_], A, U, Δ] = Unification[M, A] { type Update = U; type Delta = Δ }

  def tuple2[M[_], A, B](implicit UA: Unification[M, A], UB: Unification[M, B]): Unification.Aux[M, (A, B), UA.Update \&/ UB.Update, UA.Delta \&/ UB.Delta] =
    new Unification[M, (A, B)] {
      type Update = UA.Update \&/ UB.Update
      type Delta = UA.Delta \&/ UB.Delta

      def mustUnify(ab1: (A, B), ab2: (A, B)): M[Option[(Option[Delta], (A, B), Option[Delta])]] = ???

      def canUnify(a1: (A, B), a2: (A, B)): M[(Option[Delta], (A, B), Option[Delta])] = ???

      def dom: Dom.Aux[(A, B), Update, Delta] = Dom.tuple2(UA.dom, UB.dom)
    }
}