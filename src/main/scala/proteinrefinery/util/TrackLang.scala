package proteinrefinery.util

import nutcracker.util.{FreeK, InjectK}
import scala.language.higherKinds

private[util] sealed abstract class TrackLang[Ref[_], K[_], A] {
  type Tracked[_[_]]
}

private[util] object TrackLang {
  case class Track[Ref[_], K[_], D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]) extends TrackLang[Ref, K, Unit] { type Tracked[Var[_]] = D[Var] }
  case class Handle[Ref[_], K[_], D[_[_]]](t: DomType[D], f: Ref[D[Ref]] => K[Unit]) extends TrackLang[Ref, K, Unit] { type Tracked[Var[_]] = D[Var] }

  def track[Ref[_], K[_], D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]): TrackLang[Ref, K, Unit] = Track(t, ref)
  def handle[Ref[_], K[_], D[_[_]]](t: DomType[D], f: Ref[D[Ref]] => K[Unit]): TrackLang[Ref, K, Unit] = Handle(t, f)

  def trackF[Ref[_], F[_[_], _], D[_[_]]](ref: Ref[D[Ref]])(implicit t: DomType[D], inj: InjectK[TrackLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang[Ref, ?[_], ?], F, Unit](track[Ref, FreeK[F, ?], D](t, ref))
  def handleF[Ref[_], F[_[_], _], D[_[_]]](t: DomType[D])(f: Ref[D[Ref]] => FreeK[F, Unit])(implicit inj: InjectK[TrackLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF[TrackLang[Ref, ?[_], ?], F, Unit](handle(t, f))
}