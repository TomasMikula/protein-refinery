package proteinrefinery.lib

import scala.language.higherKinds

import nutcracker.util.{DeepEqualK, IsEqual}
import scalaz.Show

case class CompetitiveBinding[Ref[_]](
  base: Binding[Ref],
  competing: Binding[Ref]
) {
  override def toString = s"Binding ${competing} competes with binding ${base}"
}

object CompetitiveBinding {
  implicit val deepEqualKInstance: DeepEqualK[CompetitiveBinding, CompetitiveBinding] =
    new DeepEqualK[CompetitiveBinding, CompetitiveBinding] {
      def equal[Ref1[_], Ref2[_]](a1: CompetitiveBinding[Ref1], a2: CompetitiveBinding[Ref2]): IsEqual[Ref1, Ref2] =
        IsEqual(a1.base, a2.base) && IsEqual(a1.competing, a2.competing)
    }

  implicit def showInstance[Ref[_]]: Show[CompetitiveBinding[Ref]] = new Show[CompetitiveBinding[Ref]] {
    override def shows(c: CompetitiveBinding[Ref]): String = c.toString
  }
}