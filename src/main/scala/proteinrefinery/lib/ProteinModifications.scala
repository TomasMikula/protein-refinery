package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Promise.Completed
import nutcracker.{Dom, Join}
import nutcracker.syntax.dom._
import nutcracker.util.{DeepEqualK, EqualK, IsEqual}
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

  def addModification(site: Site.Definite, state: SiteState): ProteinModifications[Ref] =
    addModification(ISite[Ref](site), state)

  def addModification(site: ISite[Ref], state: SiteState): ProteinModifications[Ref] = {
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

  def from[Ref[_]](mods: (SiteLabel, SiteState)*): ProteinModifications[Ref] = {
    val mods1 = mods.foldRight[List[SiteWithState[Ref]]](Nil){ case ((s, st), sss) => SiteWithState[Ref](s, st) :: sss }
    ProteinModifications(mods1)
  }

  def apply[Ref[_]](mods: (ISite[Ref], SiteState)*): ProteinModifications[Ref] = {
    val mods1 = mods.foldRight[List[SiteWithState[Ref]]](Nil){ case ((s, st), sss) => SiteWithState[Ref](s, st) :: sss }
    ProteinModifications(mods1)
  }

  def apply[Ref[_]](mods: List[SiteWithState[Ref]]): ProteinModifications[Ref] = {
    val bag = AutoUnificationBag(mods:_*)
    ProteinModifications(bag)
  }

  /** Type that is able to uniquely identify a site within a protein. */
  sealed abstract class LocalSiteId[Ref[_]]
  case class DefiniteLabel[Ref[_]](label: Site.Definite) extends LocalSiteId[Ref]
  case class SiteRef[Ref[_]](ref: Site.Ref[Ref]) extends LocalSiteId[Ref]
  object LocalSiteId {
    def apply[Ref[_]](label: SiteLabel): LocalSiteId[Ref] = DefiniteLabel(label)
    def apply[Ref[_]](ref: Site.Ref[Ref]): LocalSiteId[Ref] = SiteRef(ref)

    implicit def equalInstance[Ref[_]](implicit ev: EqualK[Ref]): Equal[LocalSiteId[Ref]] = new Equal[LocalSiteId[Ref]] {
      def equal(a1: LocalSiteId[Ref], a2: LocalSiteId[Ref]): Boolean = (a1, a2) match {
        case (DefiniteLabel(l1), DefiniteLabel(l2)) => l1 === l2
        case (SiteRef(ref1), SiteRef(ref2)) => ref1 === ref2
        case _ => false
      }
    }

    implicit def deepEqualKInstance: DeepEqualK[LocalSiteId, LocalSiteId] =
      new DeepEqualK[LocalSiteId, LocalSiteId] {
        def equal[Ref1[_], Ref2[_]](a1: LocalSiteId[Ref1], a2: LocalSiteId[Ref2]): IsEqual[Ref1, Ref2] = (a1, a2) match {
          case (DefiniteLabel(l1), DefiniteLabel(l2)) => IsEqual(l1, l2)
          case (SiteRef(ref1), SiteRef(ref2)) => IsEqual.refs(ref1, ref2)
          case _ => IsEqual(false)
        }
      }

    implicit def showInstance[Ref[_]](implicit ev: ShowK[Ref]): Show[LocalSiteId[Ref]] = new Show[LocalSiteId[Ref]] {
      import scalaz.syntax.show._
      override def shows(id: LocalSiteId[Ref]): String = id match {
        case DefiniteLabel(s) => s.shows
        case SiteRef(ref) => s"<site#${ev.shows(ref)}>"
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

  implicit val deepEqualKInstance: DeepEqualK[ProteinModifications, ProteinModifications] =
    new DeepEqualK[ProteinModifications, ProteinModifications] {
      def equal[Ptr1[_], Ptr2[_]](pm1: ProteinModifications[Ptr1], pm2: ProteinModifications[Ptr2]): IsEqual[Ptr1, Ptr2] =
        IsEqual(pm1.mods, pm2.mods)
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
