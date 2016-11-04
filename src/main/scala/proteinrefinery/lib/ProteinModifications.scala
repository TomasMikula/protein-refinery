package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Promise.Completed
import nutcracker.{Dom, Join}
import nutcracker.syntax.dom._
import nutcracker.util.EqualK
import nutcracker.util.EqualK._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{AutoUnificationBag, ShowK, Unification}

import scalaz.{Equal, Show}
import scalaz.syntax.equal._

final case class ProteinModifications[Ref[_]] private(mods: AutoUnificationBag[SiteWithState[Ref]]) {

  lazy val isAdmissible: Boolean = !mods.list.exists(_.isFailed)

  def ifAdmissible: Option[ProteinModifications[Ref]] =
    if(isAdmissible) Some(this)
    else None

  def addModification(site: Site.Definite, state: SiteState): ProteinModifications[Ref] = {
    val (mods, _, _) = this.mods.add(SiteWithState(site, state))
    ProteinModifications(mods)
  }

  def combine(that: ProteinModifications[Ref]): ProteinModifications[Ref] = {
    val mods = this.mods union that.mods
    ProteinModifications(mods)
  }

  def refineBy(that: ProteinModifications[Ref])(implicit ev: EqualK[Ref]): (ProteinModifications[Ref], ProteinModifications.Delta[Ref]) = {
    val (mods, delta) = this.mods.addAll1(that.mods)
    (ProteinModifications(mods), delta)
  }

  def refines(that: ProteinModifications[Ref])(implicit ev: EqualK[Ref]): List[List[ProteinModifications.Update[Ref]]] = {
    val combined = (this combine that)
    if(combined =/= this) Nil
    else if(combined === that) List(Nil)
    else List(List(Join(this)))
  }

  def mentionedSites: Set[LocalSiteId[Ref]] = {
    val buf = Set.newBuilder[LocalSiteId[Ref]]
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
  type Update[Ref[_]] = Join[ProteinModifications[Ref]]
  type Delta[Ref[_]] = AutoUnificationBag.Delta[SiteWithState[Ref], SiteWithState.Delta[Ref]]

  def apply[Ref[_]](mods: (SiteLabel, SiteState)*): ProteinModifications[Ref] = {
    val mods1 = mods.foldRight[List[SiteWithState[Ref]]](Nil){ case ((s, st), sss) => SiteWithState[Ref](s, st) :: sss }
    ProteinModifications(mods1)
  }

  def apply[Ref[_]](mods: List[SiteWithState[Ref]]): ProteinModifications[Ref] = {
    val bag = AutoUnificationBag(mods:_*)
    ProteinModifications(bag)
  }

  /** Type that is able to uniquely identify a site within a protein. */
  type LocalSiteId[Ref[_]] = Either[Site.Definite, Site.Ref[Ref]]
  object LocalSiteId {
    def apply[Ref[_]](label: SiteLabel): LocalSiteId[Ref] = Left(label)
    def apply[Ref[_]](ref: Site.Ref[Ref]): LocalSiteId[Ref] = Right(ref)

    implicit def showInstance[Ref[_]](implicit ev: ShowK[Ref]): Show[LocalSiteId[Ref]] = new Show[LocalSiteId[Ref]] {
      import scalaz.syntax.show._
      override def shows(id: LocalSiteId[Ref]): String = id match {
        case Left(s) => s.shows
        case Right(ref) => s"<site#${ev.shows(ref)}>"
      }
    }
  }

  def noModifications[Ref[_]]: ProteinModifications[Ref] =
    ProteinModifications(AutoUnificationBag.empty[SiteWithState[Ref]])

  implicit def domInstance[Ref[_]](implicit ev: EqualK[Ref]): Dom.Aux[ProteinModifications[Ref], Update[Ref], Delta[Ref]] =
    new Dom[ProteinModifications[Ref]] {
      type Update = ProteinModifications.Update[Ref]
      type Delta = ProteinModifications.Delta[Ref]

      override def assess(d: ProteinModifications[Ref]): Dom.Status[Update] =
        if(d.isAdmissible) Dom.Refined
        else Dom.Failed

      override def update(d: ProteinModifications[Ref], u: Update): Option[(ProteinModifications[Ref], Delta)] = {
        val (mods, delta) = d refineBy u.value
        delta.ifNonEmpty.map((mods, _))
      }

      override def combineDeltas(d1: Delta, d2: Delta): Delta = d1 append d2
    }

  implicit def equalInstance[Ref[_]](implicit ev: EqualK[Ref]): Equal[ProteinModifications[Ref]] = new Equal[ProteinModifications[Ref]] {
    def equal(a1: ProteinModifications[Ref], a2: ProteinModifications[Ref]): Boolean =
      a1.mods === a2.mods
  }

  implicit def unificationInstance[Ref[_]](implicit ev: EqualK[Ref]): Unification.Aux[ProteinModifications[Ref], Update[Ref], Delta[Ref]] =
    new Unification[ProteinModifications[Ref]] {
      type Update = ProteinModifications.Update[Ref]
      type Delta = ProteinModifications.Delta[Ref]

      def unify(m1: ProteinModifications[Ref], m2: ProteinModifications[Ref]): (Option[Delta], ProteinModifications[Ref], Option[Delta]) = {
        val (d1, mods, d2) = (m1.mods union1 m2.mods)
        (d1.ifNonEmpty, ProteinModifications(mods), d2.ifNonEmpty)
      }

      def dom: Dom.Aux[ProteinModifications[Ref], Update, Delta] = domInstance
    }
}
