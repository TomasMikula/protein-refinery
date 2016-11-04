package proteinrefinery.util

import nutcracker.DRef
import nutcracker.util.{HHKMap, Lst, Step, WriterState}
import proteinrefinery.util.TrackLang._
import proteinrefinery.util.Tracker._

import scala.language.higherKinds
import scalaz.std.list._
import scalaz.syntax.traverse._

final case class Tracker[K](map: HHKMap[DomType, STTBuilder[K]#Out]) {

  def track[D[_[_]]](t: DomType[D], ref: DRef[D[DRef]]): (Tracker[K], Lst[K]) = {
    val tr = map.getOrElse[D](t)(SingleTypeTracker.empty[K, D])
    val ks = tr.queries.foldLeft(Lst.empty[K])((ks, f) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(refs = ref :: tr.refs))), ks)
  }

  def trackAll[D[_[_]]](t: DomType[D], refs: List[DRef[D[DRef]]]): (Tracker[K], Lst[K]) =
    Tracker.trackAll(t, refs)(this)

  def handle[D[_[_]]](t: DomType[D])(f: DRef[D[DRef]] => K): (Tracker[K], Lst[K]) = {
    val tr = map.getOrElse[D](t)(SingleTypeTracker.empty[K, D])
    val ks = tr.refs.foldLeft(Lst.empty[K])((ks, ref) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(queries = f :: tr.queries))), ks)
  }

}

object Tracker {
  sealed trait STTBuilder[K] { type Out[D[_[_]]] = SingleTypeTracker[K, D] }

  private[proteinrefinery] final case class SingleTypeTracker[K, D[_[_]]](refs: List[DRef[D[DRef]]], queries: List[DRef[D[DRef]] => K])
  private[proteinrefinery] object SingleTypeTracker {
    def empty[K, D[_[_]]] = SingleTypeTracker[K, D](Nil, Nil)
  }

  def empty[K]: Tracker[K] = Tracker(HHKMap[DomType, STTBuilder[K]#Out])

  def track[K, D[_[_]]](t: DomType[D], ref: DRef[D[DRef]]): scalaz.State[Tracker[K], Lst[K]] =
    scalaz.State(tr => tr.track(t, ref))

  def trackAll[K, D[_[_]]](t: DomType[D], refs: List[DRef[D[DRef]]]): scalaz.State[Tracker[K], Lst[K]] =
    refs.traverseS(track[K, D](t, _)).map(_.foldLeft(Lst.empty[K])(_ ++ _))

  def interpreter: Step[TrackLang, Tracker] = new Step[TrackLang, Tracker] {
    def apply[K[_], A](t: TrackLang[K, A]): WriterState[Lst[K[Unit]], Tracker[K[Unit]], A] =
      WriterState(tracker => t match {
        case i @ Track(t, ref) => tracker.track[i.Tracked](t, ref) match { case (tr, ks) => (ks, tr, ()) }
        case i @ Handle(t, f) => tracker.handle[i.Tracked](t)(f) match { case (tr, ks) => (ks, tr, ()) }
      })
  }
}