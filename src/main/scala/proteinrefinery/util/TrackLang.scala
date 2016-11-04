package proteinrefinery.util

import nutcracker.util.{FreeK, FunctorKA, InjectK}
import nutcracker.{Alternator, Antichain, DRef, Dom, DomSet, Propagation, Revocable, Trigger}

import scala.language.higherKinds
import scalaz.{Monad, ~>}
import scalaz.syntax.monad._

sealed abstract class TrackLang[K[_], A] {
  type Tracked[_[_]]
}

object TrackLang {
  final case class Track[K[_], D[_[_]]](t: DomType[D], ref: DRef[D[DRef]]) extends TrackLang[K, Unit] { type Tracked[Ref[_]] = D[Ref] }
  final case class Handle[K[_], D[_[_]]](t: DomType[D], f: DRef[D[DRef]] => K[Unit]) extends TrackLang[K, Unit] { type Tracked[Ref[_]] = D[Ref] }

  def track[K[_], D[_[_]]](t: DomType[D], ref: DRef[D[DRef]]): TrackLang[K, Unit] = Track(t, ref)
  def handle[K[_], D[_[_]]](t: DomType[D], f: DRef[D[DRef]] => K[Unit]): TrackLang[K, Unit] = Handle(t, f)

  def trackF[F[_[_], _], D[_[_]]](ref: DRef[D[DRef]])(implicit t: DomType[D], inj: InjectK[TrackLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang, F, Unit](track[FreeK[F, ?], D](t, ref))
  def handleF[F[_[_], _], D[_[_]]](t: DomType[D])(f: DRef[D[DRef]] => FreeK[F, Unit])(implicit inj: InjectK[TrackLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang, F, Unit](handle(t, f))

  implicit def functorKAInstance: FunctorKA[TrackLang] = new FunctorKA[TrackLang] {
    def transform[K[_], L[_], A](inst: TrackLang[K, A])(f: K ~> L): TrackLang[L, A] = inst match {
      case i @ Track(t, ref) => Track[L, i.Tracked](t, ref)
      case i @ Handle(t, h) => Handle[L, i.Tracked](t, h andThen (f.apply))
    }
  }

  implicit def freeTracking[F[_[_], _]](implicit i: InjectK[TrackLang, F]): Tracking[FreeK[F, ?], DRef] =
    new Tracking[FreeK[F, ?], DRef] {
      def track[D[_[_]]](ref: DRef[D[DRef]])(implicit t: DomType[D]): FreeK[F, Unit] = trackF[F, D](ref)
      def handle[D[_[_]]](t: DomType[D])(f: (DRef[D[DRef]]) => FreeK[F, Unit]): FreeK[F, Unit] = handleF(t)(f)
    }
}

trait Tracking[M[_], Ref[_]] {
  def track[D[_[_]]](ref: Ref[D[Ref]])(implicit t: DomType[D]): M[Unit]
  def handle[D[_[_]]](t: DomType[D])(f: Ref[D[Ref]] => M[Unit]): M[Unit]


  def thresholdQuery[D[_[_]]](t: DomType[D])(f: D[Ref] => OnceTrigger[Ref[D[Ref]] => M[Unit]])(implicit
    P: Propagation[M, Ref],
    dom: Dom[D[Ref]]
  ): M[Unit] =
    handle[D](t)(ref => P.valTrigger(ref)(d => f(d) match {
      case OnceTrigger.Sleep() => Trigger.sleep[M]
      case OnceTrigger.Discard() => Trigger.discard[M]
      case OnceTrigger.Fire(h) => Trigger.fire[M](h(ref))
    }))

  def dynamicQuery[D[_[_]], Q](t: DomType[D])(qref: Ref[Q])(rel: QueryRel[Q, D[Ref]])(implicit
    P: Propagation[M, Ref],
    domD: Dom[D[Ref]],
    domQ: Dom[Q],
    M: Monad[M]
  ): M[Ref[DomSet[Ref, Revocable[Ref[D[Ref]]]]]] = for {
    res <- DomSet.init[M, Ref, Revocable[Ref[D[Ref]]]]
    _ <- handle[D](t)(dref =>
      P.alternate[Q, D[Ref], Unit, Ref[Revocable[Ref[D[Ref]]]]](qref, dref)(
        (q, d) => rel(q, d) match {
          case QueryRel.Match => Alternator.Left
          case QueryRel.NoMatch => Alternator.Right
          case QueryRel.Irreconcilable => Alternator.Stop
        },
        onStartLeft = () => M.pure(()),
        onStartRight = () => Revocable.init[M, Ref, Ref[D[Ref]]](dref) >>! { DomSet.insert(_, res) },
        onSwitchToLeft = revref => Revocable.revoke(revref),
        onSwitchToRight = (_: Unit) => Revocable.init[M, Ref, Ref[D[Ref]]](dref) >>! { DomSet.insert(_, res) },
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

  trait AntichainDomType[A[_[_]]] extends proteinrefinery.util.DomType[λ[α[_] => Antichain[A[α]]]] { self: Singleton =>
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