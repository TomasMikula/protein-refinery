package protein.mechanism

import scalaz.Show

case class Phosphorylation(
  assoc: Assoc,
  phosphoSite: Site
) {
  override def toString = s"${assoc.bindings.head.left} phosphorylates ${assoc.bindings.last.right} at $phosphoSite (via $assoc)"
}

object Phosphorylation {
  implicit def showInstance: Show[Phosphorylation] = new Show[Phosphorylation] {
    override def shows(p: Phosphorylation): String = p.toString
  }
}