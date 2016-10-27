package proteinrefinery.util

import nutcracker.util.{FreeK, FunctorKA, InjectK}
import nutcracker.{Alternator, Antichain, DRef, DomSet, Propagation, Revocable, Trigger}

import scala.language.higherKinds
import scalaz.{Monad, ~>}
import scalaz.syntax.monad._

sealed abstract class TrackLang[K[_], A]

object TrackLang {
  final case class Track[K[_], D, U, Δ](t: DomType.Aux[D, U, Δ], ref: DRef.Aux[D, U, Δ]) extends TrackLang[K, Unit]
  final case class Handle[K[_], D, U, Δ](t: DomType.Aux[D, U, Δ], f: DRef.Aux[D, U, Δ] => K[Unit]) extends TrackLang[K, Unit]

  def track[K[_], D, U, Δ](t: DomType.Aux[D, U, Δ], ref: DRef.Aux[D, U, Δ]): TrackLang[K, Unit] = Track(t, ref)
  def handle[K[_], D, U, Δ](t: DomType.Aux[D, U, Δ], f: DRef.Aux[D, U, Δ] => K[Unit]): TrackLang[K, Unit] = Handle(t, f)

  def trackF[F[_[_], _], D](ref: DRef[D])(implicit t: DomType.Aux[D, ref.Update, ref.Delta], inj: InjectK[TrackLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang, F, Unit](track[FreeK[F, ?], D, ref.Update, ref.Delta](t, ref))
  def handleF[F[_[_], _], D, U, Δ](t: DomType.Aux[D, U, Δ])(f: DRef.Aux[D, U, Δ] => FreeK[F, Unit])(implicit inj: InjectK[TrackLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang, F, Unit](handle(t, f))

  implicit def functorKAInstance: FunctorKA[TrackLang] = new FunctorKA[TrackLang] {
    def transform[K[_], L[_], A](inst: TrackLang[K, A])(f: K ~> L): TrackLang[L, A] = inst match {
      case Track(t, ref) => Track(t, ref)
      case Handle(t, h) => Handle(t, h andThen (f.apply))
    }
  }

  implicit def freeTracking[F[_[_], _]](implicit i: InjectK[TrackLang, F]): Tracking[FreeK[F, ?]] =
    new Tracking[FreeK[F, ?]] {
      def track[D](ref: DRef[D])(implicit t: DomType.Aux[D, ref.Update, ref.Delta]): FreeK[F, Unit] = trackF[F, D](ref)
      def handle[D, U, Δ](t: DomType.Aux[D, U, Δ])(f: (DRef.Aux[D, U, Δ]) => FreeK[F, Unit]): FreeK[F, Unit] = handleF(t)(f)
    }
}

trait Tracking[M[_]] {
  def track[D](ref: DRef[D])(implicit t: DomType.Aux[D, ref.Update, ref.Delta]): M[Unit]
  def handle[D, U, Δ](t: DomType.Aux[D, U, Δ])(f: DRef.Aux[D, U, Δ] => M[Unit]): M[Unit]


  def thresholdQuery[D](t: DomType[D])(f: D => OnceTrigger[DRef.Aux[D, t.Update, t.Delta] => M[Unit]])(implicit
    P: Propagation[M]
  ): M[Unit] =
    handle[D, t.Update, t.Delta](t)(ref => P.valTrigger(ref)(d => f(d) match {
      case OnceTrigger.Sleep() => Trigger.sleep[M]
      case OnceTrigger.Discard() => Trigger.discard[M]
      case OnceTrigger.Fire(h) => Trigger.fire[M](h(ref))
    }))

  def dynamicQuery[D, Q](t: DomType[D])(qref: DRef[Q])(rel: QueryRel[Q, D])(implicit
    P: Propagation[M],
    M: Monad[M]
  ): M[DomSet.Ref[Revocable[DRef[D]]]] = for {
    res <- DomSet.init[M, Revocable[DRef[D]]]
    _ <- handle[D, t.Update, t.Delta](t)(dref =>
      P.alternate[Q, D, Unit, Revocable.Ref[DRef[D]]](qref, dref)(
        (q, d) => rel(q, d) match {
          case QueryRel.Match => Alternator.Left
          case QueryRel.NoMatch => Alternator.Right
          case QueryRel.Irreconcilable => Alternator.Stop
        },
        onStartLeft = () => M.pure(()),
        onStartRight = () => Revocable.init[M, DRef[D]](dref) >>! { DomSet.insert(_, res) },
        onSwitchToLeft = revref => Revocable.revoke(revref),
        onSwitchToRight = (_: Unit) => Revocable.init[M, DRef[D]](dref) >>! { DomSet.insert(_, res) },
        onStop = (_ match {
          case Some(Right(revref)) => Revocable.revoke(revref)
          case _ => M.pure(())
        })
      )
    )
  } yield res
}

object Tracking {
  def apply[M[_]](implicit ev: Tracking[M]): Tracking[M] = ev
}

trait DomType[D] { self: Singleton =>
  type Update
  type Delta
}

object DomType {
  type Aux[D, U, Δ] = DomType[D] { type Update = U; type Delta = Δ }

  trait AntichainDomType[A] extends proteinrefinery.util.DomType[Antichain[A]] { self: Singleton =>
    override type Update = Antichain.Update[A]
    override type Delta = Antichain.Delta[A]
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