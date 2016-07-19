package proteinrefinery.util

import nutcracker.DRef
import nutcracker.util.{K3Map, Lst, Step, WriterState}
import proteinrefinery.util.TrackLang._
import proteinrefinery.util.Tracker.SingleTypeTracker

import scala.language.higherKinds
import scalaz.std.list._
import scalaz.syntax.traverse._

final case class Tracker[K](map: K3Map[DomType.Aux, SingleTypeTracker[K, ?, ?, ?]]) {

  def track[D, U, Δ](t: DomType.Aux[D, U, Δ], ref: DRef.Aux[D, U, Δ]): (Tracker[K], Lst[K]) = {
    val tr = map.getOrElse[D, U, Δ](t)(SingleTypeTracker.empty[K, D, U, Δ])
    val ks = tr.queries.foldLeft(Lst.empty[K])((ks, f) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(refs = ref :: tr.refs))), ks)
  }

  def trackAll[D, U, Δ](t: DomType.Aux[D, U, Δ], refs: List[DRef.Aux[D, U, Δ]]): (Tracker[K], Lst[K]) =
    Tracker.trackAll(t, refs)(this)

  def handle[D, U, Δ](t: DomType.Aux[D, U, Δ])(f: DRef.Aux[D, U, Δ] => K): (Tracker[K], Lst[K]) = {
    val tr = map.getOrElse[D, U, Δ](t)(SingleTypeTracker.empty[K, D, U, Δ])
    val ks = tr.refs.foldLeft(Lst.empty[K])((ks, ref) => f(ref) :: ks)
    (Tracker(map.put(t)(tr.copy(queries = f :: tr.queries))), ks)
  }

}

object Tracker {
  private[proteinrefinery] final case class SingleTypeTracker[K, D, U, Δ](refs: List[DRef.Aux[D, U, Δ]], queries: List[DRef.Aux[D, U, Δ] => K])
  private[proteinrefinery] object SingleTypeTracker {
    def empty[K, D, U, Δ] = SingleTypeTracker[K, D, U, Δ](Nil, Nil)
  }

  def empty[K]: Tracker[K] = Tracker(K3Map[DomType.Aux, SingleTypeTracker[K, ?, ?, ?]])

  def track[K, D, U, Δ](t: DomType.Aux[D, U, Δ], ref: DRef.Aux[D, U, Δ]): scalaz.State[Tracker[K], Lst[K]] =
    scalaz.State(tr => tr.track(t, ref))

  def trackAll[K, D, U, Δ](t: DomType.Aux[D, U, Δ], refs: List[DRef.Aux[D, U, Δ]]): scalaz.State[Tracker[K], Lst[K]] =
    refs.traverseS(track[K, D, U, Δ](t, _)).map(_.foldLeft(Lst.empty[K])(_ ++ _))

  def interpreter: Step[TrackLang, Tracker] = new Step[TrackLang, Tracker] {
    def apply[K[_], A](t: TrackLang[K, A]): WriterState[Lst[K[Unit]], Tracker[K[Unit]], A] =
      WriterState(tracker => t match {
        case Track(t, ref) => tracker.track(t, ref) match { case (tr, ks) => (ks, tr, ()) }
        case Handle(t, f) => tracker.handle(t)(f) match { case (tr, ks) => (ks, tr, ()) }
      })
  }
}