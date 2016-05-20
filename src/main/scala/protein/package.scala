import scala.language.higherKinds

import monocle.Lens
import nutcracker._
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.{ConstK, CoproductK, FreeK, ProductK}
import nutcracker.util.ProductK._
import protein.KBLang.KBLangK
import scalaz._
import scalaz.Id._

package object protein {
  type KB_K[K[_]] = KB

  type CostL[K[_], A] = CostLang[Cost, K, A]
  type CostS[K[_]] = ConstK[Cost, K]

  type Vocabulary0[K[_], A] = CoproductK[PropagationLang, CostL, K, A]
  type Vocabulary[K[_], A] = CoproductK[KBLangK, Vocabulary0, K, A]
  type State[K[_]] = ProductK[PropagationStore, CostS, K]

  val interpreter = (KB.interpreter :+: (PropagationStore.interpreter :*: CostLang.interpreter[Cost]).hoistId[Reader[KB, ?]]).freeInstance(implicitly, Kleisli.kleisliBindRec[Id, KB])
  def propStore[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[protein] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] = PropagationStore.naiveAssess(propStore[Q])
  private def fetch: Promised ~> (State[Q] => ?) = new ~>[Promised, State[Q] => ?] {
    def apply[A](pa: Promised[A]): (State[Q] => A) = s => propStore[Q].get(s).fetchResult(pa).get
  }
  private def getCost: State[Q] => Cost = s => cost[Q].get(s)
  private def emptyState: State[Q] = PropagationStore.empty[Q] :*: (NonDecreasingMonoid[Cost].zero: CostS[Q])

  def dfsSolver: DFSSolver[Vocabulary, State, Reader[KB, ?], Promised] =
    new DFSSolver[Vocabulary, State, Reader[KB, ?], Promised](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Reader[KB, ?], Promised, Cost] =
    new BFSSolver[Vocabulary, State, Reader[KB, ?], Promised, Cost](interpreter, emptyState, naiveAssess, fetch, getCost)

  type Cont[A] = scalaz.Cont[FreeK[Vocabulary, Unit], A]
  object Cont {
    def wrapEffect[A](c: FreeK[Vocabulary, Cont[A]]): Cont[A] =
      scalaz.Cont[FreeK[Vocabulary, Unit], A](f => c >>= { k => k(f) })
  }
}
