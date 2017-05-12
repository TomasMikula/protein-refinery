package proteinrefinery.util

import nutcracker.util.{FreeK, Inject}
import scalaz.Leibniz
import scalaz.Leibniz.===

private[util] sealed abstract class TrackLang[Ref[_], K[_], A] {
  import TrackLang._

  type Tracked[_[_]]

  def witness: Unit === A

  def fold[B](
    caseTrack: Track[Ref, K, Tracked] => B,
    caseHandle: Handle[Ref, K, Tracked] => B
  ): B
}

private[util] object TrackLang {
  case class Track[Ref[_], K[_], D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]) extends TrackLang[Ref, K, Unit] {
    type Tracked[Var[_]] = D[Var]

    override def witness: Unit === Unit = Leibniz.refl

    def fold[B](caseTrack: Track[Ref, K, D] => B, caseHandle: Handle[Ref, K, D] => B): B =
      caseTrack(this)
  }

  case class Handle[Ref[_], K[_], D[_[_]]](t: DomType[D], f: Ref[D[Ref]] => K[Unit]) extends TrackLang[Ref, K, Unit] {
    type Tracked[Var[_]] = D[Var]

    override def witness: Unit === Unit = Leibniz.refl

    override def fold[B](caseTrack: Track[Ref, K, D] => B, caseHandle: Handle[Ref, K, D] => B): B =
      caseHandle(this)
  }

  def track[Ref[_], K[_], D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]): TrackLang[Ref, K, Unit] = Track(t, ref)
  def handle[Ref[_], K[_], D[_[_]]](t: DomType[D], f: Ref[D[Ref]] => K[Unit]): TrackLang[Ref, K, Unit] = Handle(t, f)

  def trackF[Ref[_], F[_[_], _], D[_[_]]](ref: Ref[D[Ref]])(implicit t: DomType[D], inj: Inject[TrackLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(track[Ref, FreeK[F, ?], D](t, ref)))
  def handleF[Ref[_], F[_[_], _], D[_[_]]](t: DomType[D])(f: Ref[D[Ref]] => FreeK[F, Unit])(implicit inj: Inject[TrackLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(handle[Ref, FreeK[F, ?], D](t, f)))
}