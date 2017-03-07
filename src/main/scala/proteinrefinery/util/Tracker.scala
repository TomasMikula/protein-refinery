package proteinrefinery.util

import nutcracker.util.{FreeK, HHKMap, InjectK, Lst, Step, WriterState}
import proteinrefinery.util.TrackLang._
import proteinrefinery.util.Tracker._
import scala.language.higherKinds
import scalaz.std.list._
import scalaz.syntax.traverse._

private[util] class TrackingModuleImpl[Ref[_]] extends PersistentTrackingModule[Ref] {
  type Lang[K[_], A] = TrackLang[Ref, K, A]
  type State[K[_]] = Tracker[Ref, K]

  def empty[K[_]]: Tracker[Ref, K] = Tracker.empty[Ref, K]

  def stashable: StashTrackingModule[Ref] { type Lang[K[_], A] = TrackLang[Ref, K, A] } =
    new TrackingListModule[Ref, Lang, State](this)

  def freeTracking[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], Ref] =
    new Tracking[FreeK[F, ?], Ref] {
      def track[D[_[_]]](ref: Ref[D[Ref]])(implicit t: DomType[D]): FreeK[F, Unit] = trackF[Ref, F, D](ref)
      def handle[D[_[_]]](t: DomType[D])(f: (Ref[D[Ref]]) => FreeK[F, Unit]): FreeK[F, Unit] = handleF(t)(f)
    }

  def interpreter: Step[Lang, State] = new Step[Lang, State] {
    def apply[K[_], A](t: TrackLang[Ref, K, A]): WriterState[Lst[K[Unit]], Tracker[Ref, K], A] =
      WriterState(tracker => t match {
        case i @ Track(t, ref) => tracker.track[i.Tracked](t, ref) match { case (tr, ks) => (ks, tr, ()) }
        case i @ Handle(t, f) => tracker.handle[i.Tracked](t)(f) match { case (tr, ks) => (ks, tr, ()) }
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