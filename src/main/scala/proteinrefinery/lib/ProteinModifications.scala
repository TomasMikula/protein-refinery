package proteinrefinery.lib

import nutcracker.Promise.Completed
import nutcracker.{Dom, Join}
import nutcracker.syntax.dom._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{AutoUnificationBag, Unification}

import scalaz.{Equal, Show}
import scalaz.syntax.equal._

final case class ProteinModifications private(mods: AutoUnificationBag[SiteWithState]) {

  lazy val isAdmissible: Boolean = !mods.list.exists(_.isFailed)

  def ifAdmissible: Option[ProteinModifications] =
    if(isAdmissible) Some(this)
    else None

  def addModification(site: Site.Definite, state: SiteState): ProteinModifications = {
    val (mods, _, _) = this.mods.add(SiteWithState(site, state))
    ProteinModifications(mods)
  }

  def combine(that: ProteinModifications): ProteinModifications = {
    val mods = this.mods union that.mods
    ProteinModifications(mods)
  }

  def refineBy(that: ProteinModifications): (ProteinModifications, ProteinModifications.Delta) = {
    val (mods, delta) = this.mods.addAll1(that.mods)
    (ProteinModifications(mods), delta)
  }

  def refines(that: ProteinModifications): List[List[ProteinModifications.Update]] = {
    val combined = (this combine that)
    if(combined =/= this) Nil
    else if(combined === that) List(Nil)
    else List(List(Join(this)))
  }

  def mentionedSites: Set[LocalSiteId] = {
    val buf = Set.newBuilder[LocalSiteId]
    mods.foreach({
      case SiteWithState(ISite(site, refs), state) =>
        site match {
          case Completed(s) => buf += LocalSiteId(s)
          case _ =>
        }
        refs.foreach(buf += LocalSiteId(_))
    })
    buf.result()
  }

}

object ProteinModifications {
  type Update = Join[ProteinModifications]
  type Delta = AutoUnificationBag.Delta[SiteWithState, SiteWithState.Delta]

  def apply(mods: (SiteLabel, SiteState)*): ProteinModifications = {
    val mods1 = mods.map({ case (s, st) => SiteWithState(s, st) })
    val bag = AutoUnificationBag(mods1:_*)
    ProteinModifications(bag)
  }

  /** Type that is able to uniquely identify a site within a protein. */
  type LocalSiteId = Either[Site.Definite, Site.Ref]
  object LocalSiteId {
    def apply(label: SiteLabel): LocalSiteId = Left(label)
    def apply(ref: Site.Ref): LocalSiteId = Right(ref)

    implicit def showInstance: Show[LocalSiteId] = new Show[LocalSiteId] {
      import scalaz.syntax.show._
      override def shows(id: LocalSiteId): String = id match {
        case Left(s) => s.shows
        case Right(ref) => s"<site#${ref.shows}>"
      }
    }
  }

  def noModifications: ProteinModifications =
    ProteinModifications(AutoUnificationBag.empty[SiteWithState])

  implicit def domInstance: Dom.Aux[ProteinModifications, Update, Delta] =
    new Dom[ProteinModifications] {
      type Update = ProteinModifications.Update
      type Delta = ProteinModifications.Delta

      override def assess(d: ProteinModifications): Dom.Status[Update] =
        if(d.isAdmissible) Dom.Refined
        else Dom.Failed

      override def update(d: ProteinModifications, u: Update): Option[(ProteinModifications, Delta)] = {
        val (mods, delta) = d refineBy u.value
        delta.ifNonEmpty.map((mods, _))
      }

      override def combineDeltas(d1: Delta, d2: Delta): Delta = d1 append d2
    }

  implicit def equalInstance: Equal[ProteinModifications] = new Equal[ProteinModifications] {
    def equal(a1: ProteinModifications, a2: ProteinModifications): Boolean =
      a1.mods === a2.mods
  }

  implicit def unificationInstance: Unification.Aux[ProteinModifications, Update, Delta] =
    new Unification[ProteinModifications] {
      type Update = ProteinModifications.Update
      type Delta = ProteinModifications.Delta

      def unify(m1: ProteinModifications, m2: ProteinModifications): (Option[Delta], ProteinModifications, Option[Delta]) = {
        val (d1, mods, d2) = (m1.mods union1 m2.mods)
        (d1.ifNonEmpty, ProteinModifications(mods), d2.ifNonEmpty)
      }

      def dom: Dom.Aux[ProteinModifications, Update, Delta] = domInstance
    }
}
