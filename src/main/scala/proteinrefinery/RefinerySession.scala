package proteinrefinery

import nutcracker.util.{Exists, FreeK, HEqualK, ShowK}
import proteinrefinery.lib.{PhosphoTarget, PhosphoTriple, Rule}
import scalaz.Id.Id
import scalaz.{Monad, ~>}
import scalaz.syntax.monad._

trait RefinerySession {
  type Ref[_]
  type Lang[_[_], _]
  type State[_[_]]

  type Prg[A] = FreeK[Lang, A]

  implicit val prgMonad: Monad[Prg]
  implicit val refEquality: HEqualK[Ref]
  implicit val refShow: ShowK[Ref]

  protected implicit val goalKeepingApi: GoalKeeping[Prg, Ref]

  def fetch[A](ref: Ref[A]): A
  def interpret[A](prg: Prg[A]): A
  def interpretFetch[A](prg: Prg[Ref[A]]): A = fetch(interpret(prg))

  val lib: Lib[Prg, Ref]

  implicit val deref: Ref ~> Id = λ[Ref ~> Id](fetch(_))


  def nugget(bnd: lib.BindingData): Rule.Ref[Ref] = interpret(lib.addRule(bnd.witness))
  def nugget(pt: PhosphoTriple[Ref]): Rule.Ref[Ref] = interpret(lib.addRule(PhosphoTarget[Ref](pt.kinase, pt.substrate, pt.targetSite).witness))
  def addGoal[A](p: Prg[Ref[A]]): Ref[A] = interpret(p >>! goalKeepingApi.keep)
  def getGoals: List[Exists[Ref]] = interpret(goalKeepingApi.list)
}

object RefinerySession {
  type Aux[Prg0[_], Ref0[_]] = RefinerySession { type Prg[A] = Prg0[A]; type Ref[A] = Ref0[A] }
}