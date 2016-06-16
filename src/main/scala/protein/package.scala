import scala.language.higherKinds
import monocle.Lens
import nutcracker._
import nutcracker.util.{ContF, FreeK, FreeKT, InjectK}
import nutcracker.util.CoproductK._
import nutcracker.util.KList._

import scalaz.{|>=|, ~>}
import scalaz.Id._

package object protein {
  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K] = DeferStore[Cost, K]

  type DSL[K[_], A] = (KBLang :+: PropagationLang  :++: DeferL)#Out[K, A]
  type State[K]     = (KB     :*: PropagationStore :**: DeferS)#Out[K]

  type Prg[A] = FreeK[DSL, A]

  val interpreter = (KB.interpreter :&: PropagationStore.interpreter :&&: DeferStore.interpreter[Cost]).freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K]: Lens[State[K], DeferS[K]] = implicitly[Lens[State[K], DeferS[K]]]

  private[protein] type PU = Prg[Unit]
  private implicit val monadInj: Prg |>=| FreeK[PropagationLang, ?] = FreeKT.injectionOrder[PropagationLang, DSL, Id]
  private def naiveAssess: State[PU] => Assessment[List[PU]] = PropagationStore.naiveAssess(propStore[PU])
  def fetch[D](ref: DRef[D])(s: State[PU]): D = propStore[PU].get(s).fetch(ref)
  private def fetchPromised: Promised ~> (State[PU] => ?) = new ~>[Promised, State[PU] => ?] {
    def apply[A](pa: Promised[A]): (State[PU] => A) = s => propStore[PU].get(s).fetchResult(pa).get
  }
  def initialState(kb: KB[PU]): State[PU] = kb :*: PropagationStore.empty[PU] :**: (DeferStore.empty[Cost, PU]: DeferS[PU])

  def dfsSolver(kb: KB[PU]): DFSSolver[DSL, State, Id, Promised] =
    new DFSSolver[DSL, State, Id, Promised](interpreter, initialState(kb), naiveAssess, fetchPromised)

  implicit val injectDefer = InjectK[DeferL, DSL]

  type Cont[A] = ContF[DSL, A]
  object Cont {
    def apply[A](f: (A => Prg[Unit]) => Prg[Unit]): Cont[A] =
      ContF(f)

    def noop[A]: Cont[A] =
      ContF.noop

    def sequence[A](a: Cont[A], b: Cont[A]): Cont[A] =
      ContF.sequence(a, b)

    def wrapEffect[A](c: Prg[Cont[A]]): Cont[A] =
      ContF[DSL, A](f => c >>= { k => k(f) })
  }
}
