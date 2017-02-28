package proteinrefinery

import nutcracker.util.{HEqualK, ShowK}
import scalaz.Id.Id
import scalaz.{Monad, ~>}

trait RefinerySession {
  type Prg[_]
  type Ref[_]

  implicit val prgMonad: Monad[Prg]
  implicit val refEquality: HEqualK[Ref]
  implicit val refShow: ShowK[Ref]

  def fetch[A](ref: Ref[A]): A
  def interpret[A](prg: Prg[A]): A
  def interpretFetch[A](prg: Prg[Ref[A]]): A = fetch(interpret(prg))

  val lib: Lib[Prg, Ref]

  implicit val deref: Ref ~> Id = λ[Ref ~> Id](fetch(_))

  //  def nugget(bnd: lib.BindingData): Rule.Ref[Ref] = interpret(lib.addRule(bnd.witness))
  //  def nugget(pt: PhosphoTriple[Ref]): Rule.Ref[Ref] = interpret(lib.addRule(PhosphoTarget[Ref](pt.kinase, pt.substrate, pt.targetSite).witness))
}

object RefinerySession {
  type Aux[Prg0[_], Ref0[_]] = RefinerySession { type Prg[A] = Prg0[A]; type Ref[A] = Ref0[A] }
}