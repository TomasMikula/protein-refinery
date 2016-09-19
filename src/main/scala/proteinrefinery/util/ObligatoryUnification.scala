package proteinrefinery.util

import scala.language.higherKinds

import nutcracker.Dom

trait ObligatoryUnification[M[_], A, Δ] {
  type Update

  // - unify(a, a) = Some(None, a, None)
  // - symmetry
  // - if update(a, u) = Some((b, δ)), then unify(a, b) =  Some(Some(δ), b, None)
  // - if unify(a, b) = None, unify(a, c) = None, unify(b, c) = Some(_, d, _), then
  //     unify(a, d) = None
  def unify(a1: A, a2: A): M[Option[(Option[Δ], A, Option[Δ])]]

  def dom: Dom.Aux[A, Update, Δ]
}
