import scala.language.higherKinds
import monocle.Lens
import nutcracker._
import nutcracker.util.{ContF, FreeK, FreeKT, InjectK}
import nutcracker.util.CoproductK._
import nutcracker.util.KList._
import protein.db.{DB, DBLang}

import scalaz.{|>=|, ~>}
import scalaz.Id._

package object protein {
  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K] = DeferStore[Cost, K]

  type DSL[K[_], A] = (DBLang :+: PropagationLang  :++: DeferL)#Out[K, A]
  type State[K]     = (DB     :*: PropagationStore :**: DeferS)#Out[K]

  type Prg[A] = FreeK[DSL, A]

  val interpreter = DB.interpreter :&: PropagationStore.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreterF = interpreter.freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K]: Lens[State[K], DeferS[K]] = implicitly[Lens[State[K], DeferS[K]]]

  private[protein] type PU = Prg[Unit]
  private implicit val monadInj: Prg |>=| FreeK[PropagationLang, ?] = FreeKT.injectionOrder[PropagationLang, DSL, Id]
  private def naiveAssess: State[PU] => Assessment[List[PU]] = PropagationStore.naiveAssess(propStore[PU])
  def fetch[D](ref: DRef[D])(s: State[PU]): D = propStore[PU].get(s).fetch(ref)
  private def fetchPromised: Promised ~> (State[PU] => ?) = new ~>[Promised, State[PU] => ?] {
    def apply[A](pa: Promised[A]): (State[PU] => A) = s => propStore[PU].get(s).fetchResult(pa).get
  }
  def initialState[K](db: DB[K]): State[K] = db :*: PropagationStore.empty[K] :**: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState[K]: State[K] = initialState(DB.empty[K])

  def dfsSolver(db: DB[PU]): DFSSolver[DSL, State, Id, Promised] =
    new DFSSolver[DSL, State, Id, Promised](interpreterF, initialState(db), naiveAssess, fetchPromised)

  implicit val injectDefer = InjectK[DeferL, DSL]

  type Cont[A] = ContF[DSL, A]
}
