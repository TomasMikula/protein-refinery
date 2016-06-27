package protein.capability

import protein.mechanism.{Protein, ProteinModifications, Site}
import protein.util.syntax._

case class ProteinPattern(protein: Protein, mods: ProteinModifications) {
  def isCompatibleWith(that: ProteinPattern): Boolean =
    (this.protein == that.protein) && (this.mods combine that.mods).isDefined

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
}