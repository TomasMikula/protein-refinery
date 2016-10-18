package proteinrefinery.lib

import nutcracker.Promise.{Completed, Conflict, Empty}
import nutcracker.{Antichain, Dom, Promise}
import nutcracker.syntax.dom._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteLabel._
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{Identification, Unification}

import scalaz.Show
import scalaz.std.either._
import scalaz.std.list._
import scalaz.syntax.show._

case class ProteinPattern(protein: Protein, mods: ProteinModifications) {

  def isAdmissible = mods.isAdmissible

  def isCompatibleWith(that: ProteinPattern): Boolean =
    (this.protein == that.protein) && this.isAdmissible && that.isAdmissible && (this.mods combine that.mods).isAdmissible

  def addModification(site: SiteLabel, state: SiteState): ProteinPattern =
    ProteinPattern(protein, mods.addModification(site, state))

  def mentionedSites: Set[LocalSiteId] = mods.mentionedSites

  override def toString: String = toString(Map())

  def toString(bonds: Map[LocalSiteId, Either[Unbound.type, LinkId]]): String = {
    type LinkDesc = Either[Unbound.type, LinkId]
    type LinkDom  = Promise[LinkDesc]
    type SiteAttr = (LinkDom, SiteState)

    val bonds1: List[(ISite, SiteAttr)] =
      bonds.foldLeft[List[(ISite, SiteAttr)]](Nil)((l, siteIdLink) => {
        val (siteId, link) = siteIdLink
        val siteDesc: ISite = siteId match {
          case Left(label) => ISite(label)
          case Right(ref)  => ISite(ref)
        }
        val linkDom = Promise.completed(link)
        (siteDesc, (linkDom, Promise.empty)) :: l
      })

    val mods1 = mods.mods.inject(x => (x.site, (Promise.empty[LinkDesc], x.state)))

    implicit val siteAttrUnif =
      Unification.tuple2[Promise[LinkDesc], SiteState](
        Unification.promiseUnification[LinkDesc],
        SiteState.unificationInstance)
    implicit val unif =
      Unification.tuple2[ISite, SiteAttr]
    implicit val ident =
      Identification.by[(ISite, SiteAttr), ISite](_._1)(unif, ISite.identificationInstance)

    val bag = mods1.addAll(bonds1)

    def siteStr(s: (ISite, SiteAttr)): String = {
      val (ISite(site, refs), (link, state)) = s
      val siteDesc = site match {
        case Completed(label) => label.shows
        case Empty => refs.headOption match {
          case Some(ref) => ref.shows
          case None => "???"
        }
        case Conflict => "⊥"
      }
      val stateS = state match {
        case Empty => ""
        case Completed(label) => label.value
        case Conflict => "⊥"
      }
      val linkS = link match {
        case Empty => "?"
        case Completed(lnk) => lnk match {
          case Left(_) => ""
          case Right(lnkId) => "!" + lnkId.value
        }
        case Conflict => "!⊥"
      }
      siteDesc + stateS + linkS
    }

    val str = bag.list.map(siteStr).mkString(",")

    s"${protein.name.name}($str)"
  }
}

object ProteinPattern {
  type Update = ProteinModifications.Update
  type Delta = ProteinModifications.Delta

  type Ref = Antichain.Ref[ProteinPattern]

  def apply(p: Protein): ProteinPattern =
    ProteinPattern(p, ProteinModifications.noModifications)

  implicit def domInstance: Dom.Aux[ProteinPattern, Update, Delta] =
    new Dom[ProteinPattern] {
      type Update = ProteinPattern.Update
      type Delta = ProteinPattern.Delta

      def update(d: ProteinPattern, u: Update): Option[(ProteinPattern, Delta)] = {
        val ProteinPattern(p, mods) = d
        Dom[ProteinModifications].update(mods, u).map({ case (mods, delta) => (ProteinPattern(p, mods), delta) })
      }

      def combineDeltas(d1: Delta, d2: Delta): Delta = Dom[ProteinModifications].combineDeltas(d1, d2)

      def assess(d: ProteinPattern): Dom.Status[Update] = d.mods.assess
    }

  implicit def showInstance: Show[ProteinPattern] = new Show[ProteinPattern] {
    override def shows(pp: ProteinPattern) = pp.toString
  }
}