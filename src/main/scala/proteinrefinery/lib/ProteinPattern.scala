package proteinrefinery.lib

import nutcracker.Dom.Status
import nutcracker.{Antichain, Dom}
import nutcracker.syntax.dom._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteLabel._
import proteinrefinery.util.syntax._

import scalaz.Show
import scalaz.syntax.show._

sealed trait ProteinPattern {
  def isCompatibleWith(that: ProteinPattern): Boolean = (this, that) match {
    case (a @ AdmissibleProteinPattern(_, _), b @ AdmissibleProteinPattern(_, _)) => a isCompatibleWith b
    case _ => false
  }
}

object ProteinPattern {
  type Update = ProteinModifications.Update
  type Delta = ProteinModifications.Delta

  def apply(p: Protein, mods: ProteinModifications): ProteinPattern = mods match {
    case am @ AdmissibleProteinModifications(_, _) => AdmissibleProteinPattern(p, am)
    case InvalidProteinModifications => InvalidProteinPattern
  }

  implicit def domInstance: Dom.Aux[ProteinPattern, Update, Delta] =
    new Dom[ProteinPattern] {
      type Update = ProteinPattern.Update
      type Delta = ProteinPattern.Delta

      def update(d: ProteinPattern, u: Update): Option[(ProteinPattern, Delta)] = d match {
        case AdmissibleProteinPattern(p, mods) =>
          Dom[ProteinModifications].update(mods, u).map({ case (mods, delta) => (ProteinPattern(p, mods), delta) })
        case InvalidProteinPattern =>
          None
      }

      def combineDeltas(d1: Delta, d2: Delta): Delta = Dom[ProteinModifications].combineDeltas(d1, d2)

      def assess(d: ProteinPattern): Status[Update] = d match {
        case AdmissibleProteinPattern(p, mods) => (mods: ProteinModifications).assess
        case InvalidProteinPattern => Dom.Failed
      }
    }
}

case object InvalidProteinPattern extends ProteinPattern

case class AdmissibleProteinPattern(protein: Protein, mods: AdmissibleProteinModifications) extends ProteinPattern {
  def isCompatibleWith(that: AdmissibleProteinPattern): Boolean =
    (this.protein == that.protein) && (this.mods combine that.mods).isAdmissible

  def addModification(site: SiteLabel, state: SiteState): Option[AdmissibleProteinPattern] =
    mods.addModification(site, state).toOption.map(AdmissibleProteinPattern(protein, _))

  def mentionedSites: Set[LocalSiteId] = mods.mentionedSites

  override def toString: String = toString(Map())

  def toString(bonds: Map[LocalSiteId, Either[Unbound.type, LinkId]]): String = {
    val (definiteBonds, nonDefiniteBonds) = bonds.splitKeys(identity)

    def siteString[SiteId: Show](modsBonds: Map[SiteId, (Option[SiteState], Option[Either[Unbound.type, LinkId]])]): String =
      modsBonds.iterator.map[String]({
        case (site, state_link) => state_link match {
          case (Some(state), Some(Right(link))) => s"${site.shows}~${state.label}!${link.value}"
          case (Some(state), Some(Left(_))) => s"${site.shows}~${state.label}"
          case (Some(state), None) => s"${site.shows}~${state.label}?"
          case (None, Some(Right(link))) => s"${site.shows}!${link.value}"
          case (None, Some(Left(_))) => s"${site.shows}"
          case (None, None) => sys.error("unreachable code")
        }
      }).mkString(",")

    val    definiteSiteString = siteString[Site.Definite](mods.   finalSiteMods.mods.mapValues(_._1).pairWithOpt(   definiteBonds))
    val nonDefiniteSiteString = siteString[Site.Ref     ](mods.nonFinalSiteMods.mods.mapValues(_._2).pairWithOpt(nonDefiniteBonds))

    s"${protein.name.name}($definiteSiteString,$nonDefiniteSiteString)"
  }
}

object AdmissibleProteinPattern {
  type Ref = Antichain.Ref[AdmissibleProteinPattern]

  def apply(p: Protein): AdmissibleProteinPattern = AdmissibleProteinPattern(p, ProteinModifications.noModifications)

  implicit def showInstance: Show[AdmissibleProteinPattern] = new Show[AdmissibleProteinPattern] {
    override def shows(pp: AdmissibleProteinPattern) = pp.toString
  }
}