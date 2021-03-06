package proteinrefinery.util

import nutcracker.util.{FreeK, HHKMap, Inject, Lst, MonadTellState, StateInterpreter, StratifiedMonoidAggregator}
import nutcracker.util.ops._
import proteinrefinery.util.TrackLang._
import proteinrefinery.util.Tracker._
import scalaz.{Bind, Lens}
import scalaz.std.list._
import scalaz.syntax.traverse._

private[util] class TrackingModuleImpl[Ref[_[_], _], Val[_[_], _]] extends PersistentTrackingModule[Ref, Val] {
  type Lang[K[_], A] = TrackLang[Ref[K, ?], K, A]
  type StateK[K[_]] = Tracker[Ref[K, ?], K]

  def emptyK[K[_]]: Tracker[Ref[K, ?], K] = Tracker.empty[Ref[K, ?], K]

  def stashable: StashTrackingModule[Ref, Val] { type Lang[K[_], A] = TrackLang[Ref[K, ?], K, A] } =
    new TrackingListModule[Ref, Val, Lang, StateK](this)

  def freeTracking[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Tracking[FreeK[F, ?], Ref[FreeK[F, ?], ?], Val[FreeK[F, ?], ?]] =
    new Tracking[FreeK[F, ?], Ref[FreeK[F, ?], ?], Val[FreeK[F, ?], ?]] {
      type Ref1[A] = Ref[FreeK[F, ?], A]
      def track[D[_[_]]](ref: Ref1[D[Ref1]])(implicit t: DomType[D]): FreeK[F, Unit] = trackF[Ref1, F, D](ref)
      def handle[D[_[_]]](t: DomType[D])(f: (Ref1[D[Ref1]]) => FreeK[F, Unit]): FreeK[F, Unit] = handleF[Ref1, F, D](t)(f)
    }

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] = new StateInterpreter[K, Lang[K, ?], S] {
    def apply[M[_], W, A](t: TrackLang[Ref[K, ?], K, A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[TrackLang[Ref[K, ?], K, ?], K], K: Bind[K]): M[A] =
      M.writerState(s => {
        val tracker = lens.get(s)
        t.fold(
          caseTrack = i => tracker.track[i.Tracked](i.t, i.ref) match { case (tr, ks) => (ks at 0, s set tr, t.witness(())) },
          caseHandle = i => tracker.handle[i.Tracked](i.t)(i.f) match { case (tr, ks) => (ks at 0, s set tr, t.witness(())) }
        )
      })
  }
}

private[util] final case class Tracker[Ref[_], K[_]](map: HHKMap[DomType, STTBuilder[Ref, K]#Out]) {

  def track[D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]): (Tracker[Ref, K], Lst[K[Unit]]) = {
    val tr = map.getOrElse[D](t)(SingleTypeTracker.empty[Ref, K, D])
    val ks = tr.queries.foldLeft(Lst.empty[K[Unit]])((ks, f) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(refs = ref :: tr.refs))), ks)
  }

  def trackAll[D[_[_]]](t: DomType[D], refs: List[Ref[D[Ref]]]): (Tracker[Ref, K], Lst[K[Unit]]) =
    Tracker.trackAll(t, refs)(this)

  def handle[D[_[_]]](t: DomType[D])(f: Ref[D[Ref]] => K[Unit]): (Tracker[Ref, K], Lst[K[Unit]]) = {
    val tr = map.getOrElse[D](t)(SingleTypeTracker.empty[Ref, K, D])
    val ks = tr.refs.foldLeft(Lst.empty[K[Unit]])((ks, ref) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(queries = f :: tr.queries))), ks)
  }

}

private[util] object Tracker {
  sealed trait STTBuilder[Ref[_], K[_]] { type Out[D[_[_]]] = SingleTypeTracker[Ref, K, D] }

  private[proteinrefinery] final case class SingleTypeTracker[Ref[_], K[_], D[_[_]]](refs: List[Ref[D[Ref]]], queries: List[Ref[D[Ref]] => K[Unit]])
  private[proteinrefinery] object SingleTypeTracker {
    def empty[Ref[_], K[_], D[_[_]]] = SingleTypeTracker[Ref, K, D](Nil, Nil)
  }

  def empty[Ref[_], K[_]]: Tracker[Ref, K] = Tracker(HHKMap[DomType, STTBuilder[Ref, K]#Out])

  def track[Ref[_], K[_], D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]): scalaz.State[Tracker[Ref, K], Lst[K[Unit]]] =
    scalaz.State(tr => tr.track(t, ref))

  def trackAll[Ref[_], K[_], D[_[_]]](t: DomType[D], refs: List[Ref[D[Ref]]]): scalaz.State[Tracker[Ref, K], Lst[K[Unit]]] =
    refs.traverseS(track[Ref, K, D](t, _)).map(_.foldLeft(Lst.empty[K[Unit]])(_ ++ _))
}