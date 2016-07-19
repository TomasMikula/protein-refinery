package proteinrefinery.lib

import scalaz.Show

case class Phosphorylation(
  assoc: Assoc,
  phosphoSite: Site
) {
  def kinase: Protein = assoc.bindings.head.left

  override def toString = s"${assoc.bindings.head.left} phosphorylates ${assoc.bindings.last.right} at $phosphoSite (via $assoc)"
}

object Phosphorylation {
  implicit def showInstance: Show[Phosphorylation] = new Show[Phosphorylation] {
    override def shows(p: Phosphorylation): String = p.toString
  }
}