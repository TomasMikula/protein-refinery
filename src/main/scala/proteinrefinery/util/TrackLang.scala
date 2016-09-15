package proteinrefinery.util

import nutcracker.util.{FreeK, FunctorKA, InjectK}
import nutcracker.{Alternator, Antichain, DRef, DomSet, PropagationLang, Revocable, Trigger}

import scala.language.higherKinds
import scalaz.~>

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

  def thresholdQuery[F[_[_], _], D](t: DomType[D])(f: D => OnceTrigger[DRef.Aux[D, t.Update, t.Delta] => FreeK[F, Unit]])(implicit
    i: InjectK[TrackLang, F],
    j: InjectK[PropagationLang, F]
  ): FreeK[F, Unit] =
    handleF[F, D, t.Update, t.Delta](t)(ref => PropagationLang.valTriggerF(ref)(d => f(d) match {
      case OnceTrigger.Sleep() => Trigger.sleep[F]
      case OnceTrigger.Discard() => Trigger.discard[F]
      case OnceTrigger.Fire(h) => Trigger.fire[F](h(ref))
    }))

  def dynamicQuery[F[_[_], _], D, Q](t: DomType[D])(qref: DRef[Q])(rel: QueryRel[Q, D])(implicit
    i: InjectK[TrackLang, F],
    j: InjectK[PropagationLang, F]
  ): FreeK[F, DomSet.Ref[Revocable[DRef[D]]]] = for {
    res <- DomSet.init[F, Revocable[DRef[D]]]
    _ <- handleF[F, D, t.Update, t.Delta](t)(dref =>
      PropagationLang.alternate[F, Q, D, Unit, Revocable.Ref[DRef[D]]](qref, dref)(
        (q, d) => rel(q, d) match {
          case QueryRel.Match => Alternator.Left
          case QueryRel.NoMatch => Alternator.Right
          case QueryRel.Irreconcilable => Alternator.Stop
        },
        onStartLeft = () => FreeK.pure[F, Unit](()),
        onStartRight = () => Revocable.init[F, DRef[D]](dref) effect { DomSet.insert(_, res) },
        onSwitchToLeft = revref => Revocable.revoke(revref),
        onSwitchToRight = (_: Unit) => Revocable.init[F, DRef[D]](dref) effect { DomSet.insert(_, res) },
        onStop = (_ match {
          case Some(Right(revref)) => Revocable.revoke(revref)
          case _ => FreeK.pure[F, Unit](())
        })
      )
    )
  } yield res

  implicit def functorKAInstance: FunctorKA[TrackLang] = new FunctorKA[TrackLang] {
    def transform[K[_], L[_], A](inst: TrackLang[K, A])(f: K ~> L): TrackLang[L, A] = inst match {
      case Track(t, ref) => Track(t, ref)
      case Handle(t, h) => Handle(t, h andThen (f.apply))
    }
  }
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