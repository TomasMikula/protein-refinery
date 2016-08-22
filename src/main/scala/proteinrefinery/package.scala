import scala.language.higherKinds
import monocle.Lens
import nutcracker.DSet.DSetRef
import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.FreeK
import nutcracker.util.CoproductK._
import nutcracker.util.KList._
import proteinrefinery.db.{DB, DBLang}
import proteinrefinery.util.{TrackLang, Tracker}

package object proteinrefinery {
  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K] = DeferStore[Cost, K]


  type DSL[K[_], A] = (DBLang :+: PropagationLang  :++: DeferL)#Out[K, A]
  type State[K]     = (DB     :*: PropagationStore :**: DeferS)#Out[K]
  type Prg[A] = FreeK[DSL, A]
  private[proteinrefinery] type PU = Prg[Unit]

  val interpreter = DB.interpreter :&: PropagationStore.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreterF = interpreter.freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def fetch[D](ref: DRef[D])(s: State[PU]): D = propStore[PU].get(s).fetch(ref)
  def initialState[K](db: DB[K]): State[K] = db :*: PropagationStore.empty[K] :**: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState[K]: State[K] = initialState(DB.empty[K])


  type DSL2[K[_], A] = (TrackLang :+: PropagationLang  :++: DeferL)#Out[K, A]
  type State2[K]     = (Tracker   :*: PropagationStore :**: DeferS)#Out[K]
  type Prg2[A] = FreeK[DSL2, A]
  private[proteinrefinery] type PU2 = Prg2[Unit]

  val interpreter2 = Tracker.interpreter :&: PropagationStore.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreter2F = interpreter2.freeInstance
  def propStore2[K]: Lens[State2[K], PropagationStore[K]] = implicitly[Lens[State2[K], PropagationStore[K]]]
  def fetch2[D](ref: DRef[D])(s: State2[PU2]): D = propStore2[PU2].get(s).fetch(ref)
  def fetchDSet2[D](ref: DSetRef[D])(s: State2[PU2]): Set[D] = {
    val dset = fetch2(ref)(s)
    dset.refined.map(fetch2(_)(s))
  }
  def fetchIncSet2[D](ref: IncSetRef[D])(s: State2[PU2]): Set[D] = {
    fetch2(ref)(s).value
  }
  def initialState2[K](tr: Tracker[K]): State2[K] = tr :*: PropagationStore.empty[K] :**: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState2[K]: State2[K] = initialState2(Tracker.empty[K])
}
