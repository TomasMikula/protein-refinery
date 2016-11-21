package proteinrefinery

import nutcracker.util.HEqualK

import scala.language.higherKinds
import scalaz.~>
import scalaz.Id.Id

trait Refinery {

  type M[_]
  type Ref[_]

  implicit val refEquality: HEqualK[Ref]

  implicit val fetch: Ref ~> Id

  def interpret[A](prg: M[A]): A

  val lib: Lib[M, Ref]
}
