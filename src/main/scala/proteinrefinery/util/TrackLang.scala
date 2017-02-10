package proteinrefinery.util

import nutcracker.util.{FreeK, FunctorKA, InjectK}
import scala.language.higherKinds
import scalaz.~>

sealed abstract class TrackLang[Ref[_], K[_], A] {
  type Tracked[_[_]]
}

object TrackLang {
  case class Track[Ref[_], K[_], D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]) extends TrackLang[Ref, K, Unit] { type Tracked[Var[_]] = D[Var] }
  case class Handle[Ref[_], K[_], D[_[_]]](t: DomType[D], f: Ref[D[Ref]] => K[Unit]) extends TrackLang[Ref, K, Unit] { type Tracked[Var[_]] = D[Var] }

  def track[Ref[_], K[_], D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]): TrackLang[Ref, K, Unit] = Track(t, ref)
  def handle[Ref[_], K[_], D[_[_]]](t: DomType[D], f: Ref[D[Ref]] => K[Unit]): TrackLang[Ref, K, Unit] = Handle(t, f)

  def trackF[Ref[_], F[_[_], _], D[_[_]]](ref: Ref[D[Ref]])(implicit t: DomType[D], inj: InjectK[TrackLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang[Ref, ?[_], ?], F, Unit](track[Ref, FreeK[F, ?], D](t, ref))
  def handleF[Ref[_], F[_[_], _], D[_[_]]](t: DomType[D])(f: Ref[D[Ref]] => FreeK[F, Unit])(implicit inj: InjectK[TrackLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang[Ref, ?[_], ?], F, Unit](handle(t, f))

  implicit def functorKAInstance[Ref[_]]: FunctorKA[TrackLang[Ref, ?[_], ?]] = new FunctorKA[TrackLang[Ref, ?[_], ?]] {
    type Var[a] = Ref[a]
    def transform[K[_], L[_], A](inst: TrackLang[Var, K, A])(f: K ~> L): TrackLang[Var, L, A] = inst match {
      case i @ Track(t, ref) => Track[Var, L, i.Tracked](t, ref)
      case i @ Handle(t, h) => Handle[Var, L, i.Tracked](t, h andThen (f.apply))
    }
  }

  implicit def freeTracking[Ref[_], F[_[_], _]](implicit i: InjectK[TrackLang[Ref, ?[_], ?], F]): Tracking[FreeK[F, ?], Ref] =
    new Tracking[FreeK[F, ?], Ref] {
      def track[D[_[_]]](ref: Ref[D[Ref]])(implicit t: DomType[D]): FreeK[F, Unit] = trackF[Ref, F, D](ref)
      def handle[D[_[_]]](t: DomType[D])(f: (Ref[D[Ref]]) => FreeK[F, Unit]): FreeK[F, Unit] = handleF(t)(f)
    }
}