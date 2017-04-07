package proteinrefinery.util

import nutcracker.{Alternator, CellSet, Discrete, Dom, Propagation, Revocable}
import scalaz.{Functor, Monad}
import scalaz.syntax.monad._

trait Tracking[M[_], Ref[_]] {
  def track[D[_[_]]](ref: Ref[D[Ref]])(implicit t: DomType[D]): M[Unit]
  def handle[D[_[_]]](t: DomType[D])(f: Ref[D[Ref]] => M[Unit]): M[Unit]


  def thresholdQuery[D[_[_]]](t: DomType[D])(f: D[Ref] => OnceTrigger[Ref[D[Ref]] => M[Unit]])(implicit
    P: Propagation[M, Ref],
    M: Functor[M],
    dom: Dom[D[Ref]]
  ): M[Unit] =
    handle[D](t)(ref => P.observe(ref).thresholdOpt_(d => f(d) match {
      case OnceTrigger.Sleep() => None
      case OnceTrigger.Discard() => Some(None)
      case OnceTrigger.Fire(h) => Some(Some(h(ref)))
    }))

  def dynamicQuery[D[_[_]], Q](t: DomType[D])(qref: Ref[Q])(rel: QueryRel[Q, D[Ref]])(implicit
    P: Propagation[M, Ref],
    domD: Dom[D[Ref]],
    domQ: Dom[Q],
    M: Monad[M]
  ): M[Ref[CellSet[Ref, Revocable[Ref[D[Ref]]]]]] = for {
    res <- CellSet.init[Revocable[Ref[D[Ref]]]]()
    _ <- handle[D](t)(dref =>
      P.alternate[Q, D[Ref], Unit, Ref[Revocable[Ref[D[Ref]]]]](qref, dref)(
        (q, d) => rel(q, d) match {
          case QueryRel.Match => Alternator.Left
          case QueryRel.NoMatch => Alternator.Right
          case QueryRel.Irreconcilable => Alternator.Stop
        },
        onStartLeft = () => M.pure(()),
        onStartRight = () => Revocable.init[M, Ref, Ref[D[Ref]]](dref) >>! { CellSet.insert(_, res) },
        onSwitchToLeft = revref => Revocable.revoke(revref),
        onSwitchToRight = (_: Unit) => Revocable.init[M, Ref, Ref[D[Ref]]](dref) >>! { CellSet.insert(_, res) },
        onStop = (_ match {
          case Some(Right(revref)) => Revocable.revoke(revref)
          case _ => M.pure(())
        })
      )
    )
  } yield res
}

object Tracking {
  def apply[M[_], Ref[_]](implicit ev: Tracking[M, Ref]): Tracking[M, Ref] = ev
}

trait DomType[D[_[_]]] { self: Singleton =>
}

object DomType {
  type Aux[D[_[_]], U, Δ] = DomType[D] { type Update = U; type Delta = Δ }

  trait DiscreteDomType[A[_[_]]] extends DomType[λ[α[_] => Discrete[A[α]]]] { self: Singleton =>
  }
}

sealed trait OnceTrigger[A] {
  import OnceTrigger._

  def map[B](f: A => B): OnceTrigger[B] = this match {
    case Sleep() => Sleep()
    case Fire(a) => Fire(f(a))
    case Discard() => Discard()
  }
}
object OnceTrigger {
  final case class Sleep[A]() extends OnceTrigger[A]
  final case class Fire[A](a: A) extends OnceTrigger[A]
  final case class Discard[A]() extends OnceTrigger[A]
}