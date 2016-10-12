package proteinrefinery.lib

import nutcracker.Dom.Status
import nutcracker.Promise.{Completed, Conflict, Empty}
import nutcracker.{Antichain, Dom, Promise}
import nutcracker.syntax.dom._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteLabel._
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{Identification, Unification}

import scalaz.{Monad, Show}
import scalaz.Id._
import scalaz.std.either._
import scalaz.std.list._
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
    case am @ AdmissibleProteinModifications(_) => AdmissibleProteinPattern(p, am)
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

    implicit val siteAttrUnif: Unification.Aux0[SiteAttr, Id] =
      Unification.tuple2[Id, Promise[LinkDesc], SiteState](
        Unification.promiseUnification[LinkDesc],
        SiteState.unificationInstance,
        Monad[Id])
    implicit val unif: Unification.Aux0[(ISite, SiteAttr), Id] =
      Unification.tuple2[Id, ISite, SiteAttr]
    implicit val ident: Identification.Aux0[(ISite, SiteAttr), Id] =
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

object AdmissibleProteinPattern {
  type Ref = Antichain.Ref[AdmissibleProteinPattern]

  def apply(p: Protein): AdmissibleProteinPattern = AdmissibleProteinPattern(p, ProteinModifications.noModifications)

  implicit def showInstance: Show[AdmissibleProteinPattern] = new Show[AdmissibleProteinPattern] {
    override def shows(pp: AdmissibleProteinPattern) = pp.toString
  }
}