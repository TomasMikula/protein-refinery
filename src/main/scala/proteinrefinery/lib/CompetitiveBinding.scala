package proteinrefinery.lib

import scala.language.higherKinds

import scalaz.Show

case class CompetitiveBinding[Ref[_]](
  base: Binding[Ref],
  competing: Binding[Ref]
) {
  override def toString = s"Binding ${competing} competes with binding ${base}"
}

object CompetitiveBinding {
  implicit def showInstance[Ref[_]]: Show[CompetitiveBinding[Ref]] = new Show[CompetitiveBinding[Ref]] {
    override def shows(c: CompetitiveBinding[Ref]): String = c.toString
  }
}