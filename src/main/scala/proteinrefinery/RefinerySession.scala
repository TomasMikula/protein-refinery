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


class RefinerySessionImpl[Prg0[_], Ref0[_]](val refinery: Refinery.Aux[Prg0, Ref0]) extends RefinerySession {

  type Prg[A] = Prg0[A]
  type Ref[A] = Ref0[A]

  implicit val prgMonad: Monad[Prg] = refinery.prgMonad
  implicit val refEquality: HEqualK[Ref] = refinery.refEquality
  implicit val refShow: ShowK[Ref] = refinery.refShow

  var state: State[Prg] = emptyState

  def fetch[A](ref: Ref[A]): A =
    refinery.fetch(ref)(state)

  def interpret[A](prg: Prg[A]): A = {
    val (s, a) = refinery.interpret(prg, state)
    state = s
    a
  }

  val lib: Lib[Prg, Ref] = refinery.lib
}