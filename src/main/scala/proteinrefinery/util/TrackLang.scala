package proteinrefinery.util

import nutcracker.util.{FreeK, FunctorKA, InjectK}
import nutcracker.DRef

import scala.language.higherKinds
import scalaz.~>

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