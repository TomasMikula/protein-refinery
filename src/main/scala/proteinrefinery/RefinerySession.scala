package proteinrefinery

import nutcracker.util.typealigned.APair
import nutcracker.util.{DeepShow, FreeK, HOrderK, ShowK}
import proteinrefinery.lib.Rule
import scalaz.Id.Id
import scalaz.{Monad, ~>}
import scalaz.syntax.monad._

trait RefinerySession {
  type VarK[_[_], _]
  type ValK[_[_], _]
  type Lang[_[_], _]
  type StateK[_[_]]

  type Prg[A] = FreeK[Lang, A]

  type Var[A] = VarK[Prg, A]
  type Val[A] = ValK[Prg, A]
  type State = StateK[Prg]

  implicit val prgMonad: Monad[Prg]

  def varOrderK[K[_]]: HOrderK[VarK[K, ?]]
  def varShowK[K[_]]: ShowK[VarK[K, ?]]

  implicit val varOrder: HOrderK[Var] = varOrderK
  implicit val varShow: ShowK[Var] = varShowK

  protected implicit val goalKeepingApi: GoalKeeping[Prg, Var]

  def fetch[A](ref: Var[A]): A
  def interpret[A](prg: Prg[A]): A
  def interpretFetch[A](prg: Prg[Var[A]]): A = fetch(interpret(prg))

  val lib: Lib[Prg, Var, Val]

  implicit val deref: Var ~> Id = λ[Var ~> Id](fetch(_))


  def nugget(bnd: lib.BindingData): Rule.Ref[Var] = interpret(lib.addBinding(bnd)).witness
  def nugget(pt: lib.PhosphoTriple): Rule.Ref[Var] = interpret(lib.addPhosphoTarget(pt))
  def addGoal[A](p: Prg[Var[A]])(implicit ev: DeepShow[A, Var]): Var[A] = interpret(p >>! { goalKeepingApi.keep(_) })
  def getGoals: List[APair[Var, DeepShow[?, Var]]] = interpret(goalKeepingApi.list)
}

object RefinerySession {
  type Aux[Prg0[_], Ref0[_]] = RefinerySession { type Prg[A] = Prg0[A]; type Ref[A] = Ref0[A] }
}