package proteinrefinery

import nutcracker.toolkit.{FreeRefToolkit}
import nutcracker.util.typealigned.APair
import nutcracker.util.{DeepShow}
import proteinrefinery.lib.Rule
import scalaz.Id.Id
import scalaz.~>
import scalaz.syntax.monad._

trait RefinerySession extends FreeRefToolkit {

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