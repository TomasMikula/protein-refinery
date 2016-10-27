import scala.language.higherKinds
import monocle.Lens
import nutcracker.DSet.DSetRef
import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.FreeK
import nutcracker.util.CoproductK._
import nutcracker.util.KList._
import proteinrefinery.lib.Lib
import proteinrefinery.util.{TrackLang, Tracker}

package object proteinrefinery extends ImplicitConversions {
  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K] = DeferStore[Cost, K]

  type DSL[K[_], A] = (TrackLang :+: PropagationLang  :++: DeferL)#Out[K, A]
  type State[K]     = (Tracker   :*: PropagationStore :**: DeferS)#Out[K]
  type Prg[A] = FreeK[DSL, A]
  private[proteinrefinery] type PU = Prg[Unit]

  val Lib = new Lib[Prg]

  val interpreter = Tracker.interpreter :&: PropagationStore.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreterF = interpreter.freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def fetch[D](ref: DRef[D])(s: State[PU]): D = propStore[PU].get(s).fetch(ref)
  def fetchDSet[D](ref: DSetRef[D])(s: State[PU]): Set[D] = {
    val dset = fetch(ref)(s)
    dset.refined.map(fetch(_)(s))
  }
  def fetchIncSet[D](ref: IncSetRef[D])(s: State[PU]): Set[D] = {
    fetch(ref)(s).value
  }
  def initialState[K](tr: Tracker[K]): State[K] = tr :*: PropagationStore.empty[K] :**: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState[K]: State[K] = initialState(Tracker.empty[K])
}
