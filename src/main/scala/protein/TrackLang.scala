package protein

import scala.language.higherKinds
import nutcracker.{DRef, PropagationLang, Trigger}
import nutcracker.util.{FreeK, InjectK}

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

}

trait DomType[D] { self: Singleton =>
  type Update
  type Delta
}

object DomType {
  type Aux[D, U, Δ] = DomType[D] { type Update = U; type Delta = Δ }
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