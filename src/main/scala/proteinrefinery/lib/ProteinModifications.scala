package proteinrefinery.lib

import nutcracker.Promise.Completed
import nutcracker.{Dom, Join}
import nutcracker.syntax.dom._
import proteinrefinery.lib.AdmissibleProteinModifications.SiteWithState
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{AutoUnificationBag, Identification, Unification}

import scalaz.{Equal, Show}
import scalaz.Id._
import scalaz.std.tuple._
import scalaz.syntax.equal._

sealed trait ProteinModifications {

  def isAdmissible: Boolean = this match {
    case AdmissibleProteinModifications(_) => true
    case InvalidProteinModifications => false
  }

  /** `ProteinModifications` is isomorphic to `Option[AdmissibleProteinModifications]` */
  def toOption: Option[AdmissibleProteinModifications] = this match {
    case a @ AdmissibleProteinModifications(_) => Some(a)
    case InvalidProteinModifications => None
  }

  def addModification(site: SiteLabel, state: SiteState): ProteinModifications

  def combine(that: ProteinModifications): ProteinModifications = (this, that) match {
    case (a @ AdmissibleProteinModifications(_), b @ AdmissibleProteinModifications(_)) => a.combine0(b)
    case _ => InvalidProteinModifications
  }

  def refines(that: ProteinModifications): Option[List[List[ProteinModifications.Update]]] =
    (this, that) match {
      case (InvalidProteinModifications, _) => None
      case (_, InvalidProteinModifications) => Some(Nil)
      case (thiz @ AdmissibleProteinModifications(_), that @ AdmissibleProteinModifications(_)) => Some(thiz refines0 that)
    }
}

case object InvalidProteinModifications extends ProteinModifications {
  def addModification(site: Site.Definite, state: SiteState): ProteinModifications = this
}

case class AdmissibleProteinModifications private(mods: AutoUnificationBag[SiteWithState]) extends ProteinModifications {
  import AdmissibleProteinModifications._

  override def addModification(site: Site.Definite, state: SiteState): ProteinModifications = {
    val (mods, addedElem, _) = this.mods.add(SiteWithState(site, state))
    if(addedElem.isFailed) InvalidProteinModifications
    else AdmissibleProteinModifications(mods)
  }

  def combine0(that: AdmissibleProteinModifications): ProteinModifications = {
    val mods = this.mods union that.mods
    if(mods.list.exists(_.isFailed)) InvalidProteinModifications
    else AdmissibleProteinModifications(mods)
  }

  def mentionedSites: Set[LocalSiteId] = {
    val buf = Set.newBuilder[LocalSiteId]
    mods.foreach({
      case (ISite(site, refs), state) =>
        site match {
          case Completed(s) => buf += LocalSiteId(s)
          case _ =>
        }
        refs.foreach(buf += LocalSiteId(_))
    })
    buf.result()
  }

  def refines0(that: AdmissibleProteinModifications): List[List[ProteinModifications.Update]] =
    (this combine0 that) match {
      case InvalidProteinModifications => Nil
      case m @ AdmissibleProteinModifications(_) =>
        if(m =/= this) Nil
        else if(m === that) List(Nil)
        else List(List(Join(this)))
    }

}

object AdmissibleProteinModifications {

  type SiteWithState = (ISite, SiteState)
  object SiteWithState {
    def apply(s: SiteLabel, st: SiteState): SiteWithState =
      (ISite(s), st)

    implicit val equalInstance: Equal[SiteWithState] =
      scalaz.std.tuple.tuple2Equal[ISite, SiteState]
  }

  import SiteState.unificationInstance

  implicit def siteWithStateUnification: Unification.Aux0[SiteWithState, Id] =
    Unification.tuple2[Id, ISite, SiteState]

  implicit def siteWithStateIdentification: Identification.Aux0[SiteWithState, Id] =
    Identification.by[SiteWithState, ISite](_._1)(siteWithStateUnification, ISite.identificationInstance)

  def apply(mods: (SiteLabel, SiteState)*): Option[AdmissibleProteinModifications] = {
    val mods1 = mods.map({ case (s, st) => SiteWithState(s, st) })
    val bag = AutoUnificationBag(mods1:_*)
    if(bag.list.exists(siteWithStateUnification.dom.isFailed)) None
    else Some(AdmissibleProteinModifications(bag))
  }

  def noModifications: AdmissibleProteinModifications =
    AdmissibleProteinModifications(AutoUnificationBag.empty[SiteWithState])

  implicit def equalInstance: Equal[AdmissibleProteinModifications] = new Equal[AdmissibleProteinModifications] {
    implicit def setEqual[A: Equal]: Equal[Set[A]] = new Equal[Set[A]] {
      def equal(s1: Set[A], s2: Set[A]): Boolean = s1.size == s2.size && s1.forall(a1 => s2.exists(a2 => a1 === a2))
    }
    def equal(a1: AdmissibleProteinModifications, a2: AdmissibleProteinModifications): Boolean =
      a1.mods === a2.mods
  }
}

object ProteinModifications {
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

  type Update = Join[ProteinModifications]
  type Delta = Unit

  def noModifications: AdmissibleProteinModifications = AdmissibleProteinModifications.noModifications

  def apply(mods: (SiteLabel, SiteState)*): ProteinModifications =
    fromOption(AdmissibleProteinModifications(mods:_*))

  def fromOption(pm: Option[AdmissibleProteinModifications]): ProteinModifications =
    pm.fold[ProteinModifications](InvalidProteinModifications)(x => x)

  implicit def domInstance: Dom.Aux[ProteinModifications, Update, Delta] =
    new Dom[ProteinModifications] {
      type Update = ProteinModifications.Update
      type Delta = ProteinModifications.Delta
      override def assess(d: ProteinModifications): Dom.Status[Update] = d match {
        case InvalidProteinModifications => Dom.Failed
        case AdmissibleProteinModifications(_) => Dom.Refined
      }

      override def update(d: ProteinModifications, u: Update): Option[(ProteinModifications, Delta)] = {
        val res = d combine u.value
        if(res === d) None else Some((res, ()))
      }

      override def combineDeltas(d1: Delta, d2: Delta): Delta = ()
    }

  implicit def equalInstance: Equal[ProteinModifications] = new Equal[ProteinModifications] {
    def equal(a1: ProteinModifications, a2: ProteinModifications): Boolean = (a1, a2) match {
      case (a1 @ AdmissibleProteinModifications(_), a2 @ AdmissibleProteinModifications(_)) => a1 === a2
      case (InvalidProteinModifications, InvalidProteinModifications) => true
      case _ => false
    }
  }
}
