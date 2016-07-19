package proteinrefinery.lib

import scalaz.Show

case class CompetitiveBinding(
  base: Binding,
  competing: Binding
) {
  assert(base.rightPattern overlaps competing.rightPattern)

  override def toString = s"Binding ${competing} competes with binding ${base}"
}

object CompetitiveBinding {
  implicit def showInstance: Show[CompetitiveBinding] = new Show[CompetitiveBinding] {
    override def shows(c: CompetitiveBinding): String = c.toString
  }
}