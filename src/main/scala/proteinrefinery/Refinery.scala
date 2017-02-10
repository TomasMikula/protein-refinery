package proteinrefinery

import nutcracker.util.{HEqualK, ShowK}
import proteinrefinery.lib.{PhosphoTarget, PhosphoTriple, Rule}

import scala.language.higherKinds
import scalaz.~>
import scalaz.Id.Id

trait Refinery {

  type M[_]
  type Ref[_]

  implicit val refEquality: HEqualK[Ref]
  implicit val refShow: ShowK[Ref]

  implicit val fetch: Ref ~> Id

  def nugget(bnd: lib.BindingData): Rule.Ref[Ref] = interpret(lib.addRule(bnd.witness))
  def nugget(pt: PhosphoTriple[Ref]): Rule.Ref[Ref] = interpret(lib.addRule(PhosphoTarget[Ref](pt.kinase, pt.substrate, pt.targetSite).witness))

  def interpret[A](prg: M[A]): A

  val lib: Lib[M, Ref]
}
