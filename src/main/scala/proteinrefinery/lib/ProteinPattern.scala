package proteinrefinery.lib

import nutcracker.Antichain
import proteinrefinery.util.syntax._

import scalaz.Show

case class ProteinPattern(protein: Protein, mods: AdmissibleProteinModifications) {
  def isCompatibleWith(that: ProteinPattern): Boolean =
    (this.protein == that.protein) && (this.mods combine that.mods).isAdmissible

  def addModification(site: Site, state: SiteState): Option[ProteinPattern] =
    mods.addModification(site, state).toOption.map(ProteinPattern(protein, _))

  override def toString: String = toString(Map())

  def toString(bonds: Map[Site.Dom, Either[Unbound.type, LinkId]]): String = {
    val siteString = mods.finalSiteMods.mods.pairWithOpt(bonds).iterator.map[String]({
      case (site, state_link) => state_link match {
        case (Some((state, _)), Some(Right(link))) => s"${site.value.name}~${state.label}!${link.value}"
        case (Some((state, _)), Some(Left(_))) => s"${site.value.name}~${state.label}"
        case (Some((state, _)), None) => s"${site.value.name}~${state.label}?"
        case (None, Some(Right(link))) => s"${site.value.name}!${link.value}"
        case (None, Some(Left(_))) => s"${site.value.name}"
        case (None, None) => sys.error("unreachable code")
      }
    }).mkString(",")

    s"${protein.name.name}($siteString)"
  }
}

object ProteinPattern {
  type Ref = Antichain.Ref[ProteinPattern]

  def apply(p: Protein): ProteinPattern = ProteinPattern(p, ProteinModifications.noModifications)

  implicit def showInstance: Show[ProteinPattern] = new Show[ProteinPattern] {
    override def shows(pp: ProteinPattern) = pp.toString
  }
}