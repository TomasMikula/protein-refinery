import scala.language.higherKinds
import monocle.Lens
import nutcracker._
import nutcracker.util.{ContF, CoproductK, FreeK, FreeKT, InjectK, ProductK}
import nutcracker.util.ProductK._

import scalaz.{|>=|, ~>}
import scalaz.Id._

package object protein {
  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K[_]] = DeferStore[Cost, K]

  type Vocabulary0[K[_], A] = CoproductK[PropagationLang, DeferL, K, A]
  type Vocabulary[K[_], A] = CoproductK[KBLang, Vocabulary0, K, A]
  type State0[K[_]] = ProductK[PropagationStore, DeferS, K]
  type State[K[_]] = ProductK[KB, State0, K]

  val interpreter = (KB.interpreter :*: PropagationStore.interpreter :*: DeferStore.interpreter[Cost]).freeInstance
  def propStore[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K[_]]: Lens[State[K], DeferS[K]] = implicitly[Lens[State[K], DeferS[K]]]

  private[protein] type Q[A] = FreeK[Vocabulary, A]
  private implicit val monadInj: Q |>=| FreeK[PropagationLang, ?] = FreeKT.injectionOrder[PropagationLang, Vocabulary, Id]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] = PropagationStore.naiveAssess(propStore[Q])
  def fetch[D](ref: DRef[D])(s: State[Q]): D = propStore[Q].get(s).fetch(ref)
  private def fetchPromised: Promised ~> (State[Q] => ?) = new ~>[Promised, State[Q] => ?] {
    def apply[A](pa: Promised[A]): (State[Q] => A) = s => propStore[Q].get(s).fetchResult(pa).get
  }
  private def emptyState0: State0[Q] = PropagationStore.empty[Q] :*: (DeferStore.empty[Cost, Q]: DeferS[Q])
  def initialState(kb: KB[Q]): State[Q] = kb :*: emptyState0

  def dfsSolver(kb: KB[Q]): DFSSolver[Vocabulary, State, Id, Promised] =
    new DFSSolver[Vocabulary, State, Id, Promised](interpreter, initialState(kb), naiveAssess, fetchPromised)

  implicit val injectDefer = InjectK[DeferL, Vocabulary]

  type Cont[A] = ContF[Vocabulary, A]
  object Cont {
    def apply[A](f: (A => FreeK[Vocabulary, Unit]) => FreeK[Vocabulary, Unit]): Cont[A] =
      ContF(f)

    def noop[A]: Cont[A] =
      ContF.noop

    def sequence[A](a: Cont[A], b: Cont[A]): Cont[A] =
      ContF.sequence(a, b)

    def wrapEffect[A](c: FreeK[Vocabulary, Cont[A]]): Cont[A] =
      scalaz.Cont[FreeK[Vocabulary, Unit], A](f => c >>= { k => k(f) })
  }
}
