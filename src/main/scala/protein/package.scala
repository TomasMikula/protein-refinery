import scala.language.higherKinds
import monocle.Lens
import nutcracker._
import nutcracker.util.FreeK
import nutcracker.util.CoproductK._
import nutcracker.util.KList._
import protein.db.{DB, DBLang}
import protein.util.{TrackLang, Tracker}

package object protein {
  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K] = DeferStore[Cost, K]


  type DSL[K[_], A] = (DBLang :+: PropagationLang  :++: DeferL)#Out[K, A]
  type State[K]     = (DB     :*: PropagationStore :**: DeferS)#Out[K]
  type Prg[A] = FreeK[DSL, A]
  private[protein] type PU = Prg[Unit]

  val interpreter = DB.interpreter :&: PropagationStore.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreterF = interpreter.freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def fetch[D](ref: DRef[D])(s: State[PU]): D = propStore[PU].get(s).fetch(ref)
  def initialState[K](db: DB[K]): State[K] = db :*: PropagationStore.empty[K] :**: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState[K]: State[K] = initialState(DB.empty[K])


  type DSL2[K[_], A] = (TrackLang :+: PropagationLang  :++: DeferL)#Out[K, A]
  type State2[K]     = (Tracker   :*: PropagationStore :**: DeferS)#Out[K]
  type Prg2[A] = FreeK[DSL2, A]
  private[protein] type PU2 = Prg2[Unit]

  val interpreter2 = Tracker.interpreter :&: PropagationStore.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreter2F = interpreter2.freeInstance
  def propStore2[K]: Lens[State2[K], PropagationStore[K]] = implicitly[Lens[State2[K], PropagationStore[K]]]
  def fetch2[D](ref: DRef[D])(s: State2[PU2]): D = propStore2[PU2].get(s).fetch(ref)
  def initialState2[K](tr: Tracker[K]): State2[K] = tr :*: PropagationStore.empty[K] :**: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState2[K]: State2[K] = initialState2(Tracker.empty[K])
}
