package proteinrefinery.lib

import proteinrefinery.util.syntax._

import scalaz.Show

case class ProteinPattern(protein: Protein, mods: ProteinModifications) {
  def isCompatibleWith(that: ProteinPattern): Boolean =
    (this.protein == that.protein) && (this.mods combine that.mods).isDefined

  def addModification(site: Site, state: SiteState): Option[ProteinPattern] =
    mods.addModification(site, state).map(ProteinPattern(protein, _))

  override def toString: String = toString(Map())

  def toString(bonds: Map[Site, Either[Unbound.type, LinkId]]): String = {
    val siteString = mods.mods.pairWithOpt(bonds).iterator.map[String]({
      case (site, state_link) => state_link match {
        case (Some(state), Some(Right(link))) => s"${site.name}~${state.label}!${link.value}"
        case (Some(state), Some(Left(_))) => s"${site.name}~${state.label}"
        case (Some(state), None) => s"${site.name}~${state.label}?"
        case (None, Some(Right(link))) => s"${site.name}!${link.value}"
        case (None, Some(Left(_))) => s"${site.name}"
        case (None, None) => sys.error("unreachable code")
      }
    }).mkString(",")

    s"${protein.name.name}($siteString)"
  }
}

object ProteinPattern {
  def apply(p: Protein): ProteinPattern = ProteinPattern(p, ProteinModifications.noModifications)

  implicit def showInstance: Show[ProteinPattern] = new Show[ProteinPattern] {
    override def shows(pp: ProteinPattern) = pp.toString
  }
}