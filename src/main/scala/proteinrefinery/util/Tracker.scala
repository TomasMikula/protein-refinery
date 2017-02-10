package proteinrefinery.util

import nutcracker.util.{HHKMap, Lst, Step, WriterState}
import proteinrefinery.util.TrackLang._
import proteinrefinery.util.Tracker._

import scala.language.higherKinds
import scalaz.std.list._
import scalaz.syntax.traverse._

final case class Tracker[Ref[_], K](map: HHKMap[DomType, STTBuilder[Ref,   K]#Out]) {

  def track[D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]): (Tracker[Ref, K], Lst[K]) = {
    val tr = map.getOrElse[D](t)(SingleTypeTracker.empty[Ref, K, D])
    val ks = tr.queries.foldLeft(Lst.empty[K])((ks, f) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(refs = ref :: tr.refs))), ks)
  }

  def trackAll[D[_[_]]](t: DomType[D], refs: List[Ref[D[Ref]]]): (Tracker[Ref, K], Lst[K]) =
    Tracker.trackAll(t, refs)(this)

  def handle[D[_[_]]](t: DomType[D])(f: Ref[D[Ref]] => K): (Tracker[Ref, K], Lst[K]) = {
    val tr = map.getOrElse[D](t)(SingleTypeTracker.empty[Ref, K, D])
    val ks = tr.refs.foldLeft(Lst.empty[K])((ks, ref) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(queries = f :: tr.queries))), ks)
  }

}

object Tracker {
  sealed trait STTBuilder[Ref[_], K] { type Out[D[_[_]]] = SingleTypeTracker[Ref, K, D] }

  private[proteinrefinery] final case class SingleTypeTracker[Ref[_], K, D[_[_]]](refs: List[Ref[D[Ref]]], queries: List[Ref[D[Ref]] => K])
  private[proteinrefinery] object SingleTypeTracker {
    def empty[Ref[_], K, D[_[_]]] = SingleTypeTracker[Ref, K, D](Nil, Nil)
  }

  def empty[Ref[_], K]: Tracker[Ref, K] = Tracker(HHKMap[DomType, STTBuilder[Ref, K]#Out])

  def track[Ref[_], K, D[_[_]]](t: DomType[D], ref: Ref[D[Ref]]): scalaz.State[Tracker[Ref, K], Lst[K]] =
    scalaz.State(tr => tr.track(t, ref))

  def trackAll[Ref[_], K, D[_[_]]](t: DomType[D], refs: List[Ref[D[Ref]]]): scalaz.State[Tracker[Ref, K], Lst[K]] =
    refs.traverseS(track[Ref, K, D](t, _)).map(_.foldLeft(Lst.empty[K])(_ ++ _))

  def interpreter[Ref[_]]: Step[TrackLang[Ref, ?[_], ?], Tracker[Ref, ?]] = new Step[TrackLang[Ref, ?[_], ?], Tracker[Ref, ?]] {
    def apply[K[_], A](t: TrackLang[Ref, K, A]): WriterState[Lst[K[Unit]], Tracker[Ref, K[Unit]], A] =
      WriterState(tracker => t match {
        case i @ Track(t, ref) => tracker.track[i.Tracked](t, ref) match { case (tr, ks) => (ks, tr, ()) }
        case i @ Handle(t, f) => tracker.handle[i.Tracked](t)(f) match { case (tr, ks) => (ks, tr, ()) }
      })
  }
}