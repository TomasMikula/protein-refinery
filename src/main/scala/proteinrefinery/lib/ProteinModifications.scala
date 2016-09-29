package proteinrefinery.lib

import nutcracker.Dom.Aux
import nutcracker.Promise.Completed
import nutcracker.{Dom, Join}
import proteinrefinery.lib.AdmissibleProteinModifications.SiteWithState
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.util.{AutoUnificationBag, Unification}

import scalaz.Id.Id
import scalaz.std.option._
import scalaz.{Monad, MonadPartialOrder, Show}

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

case class AdmissibleProteinModifications(mods: AutoUnificationBag[Option, SiteWithState]) extends ProteinModifications {
  import AdmissibleProteinModifications._

  override def addModification(site: Site.Definite, state: SiteState): ProteinModifications =
    ProteinModifications.fromOption(mods.add(SiteWithState(site, state)).map({ case (mods, _, _) => AdmissibleProteinModifications(mods) }))

  def combine0(that: AdmissibleProteinModifications): ProteinModifications =
    ProteinModifications.fromOption((this.mods union that.mods).map(AdmissibleProteinModifications(_)))

  def mentionedSites: Set[LocalSiteId] = {
    val buf = Set.newBuilder[LocalSiteId]
    mods.foreach({
      case ((site, refs), state) =>
        site match {
          case Completed(s) => buf += LocalSiteId(s)
          case _ =>
        }
        refs.foreach(buf += LocalSiteId(_))
    })
    buf.result()
  }

  def refines0(that: AdmissibleProteinModifications): List[List[ProteinModifications.Update]] = ???

}

object AdmissibleProteinModifications {

  type SiteWithState = ((Site.Dom, Set[Site.Ref]), SiteState)
  object SiteWithState {
    def apply(s: SiteLabel, st: SiteState): SiteWithState =
      ((Site.fromLabel(s), Set()), st)
  }

  private implicit def setUnificationByNonEmptyIntersection[A]: Unification[Id, Set[A]] = new Unification[Id, Set[A]] {
    type Update = Set[A] // what to add
    type Delta = Set[A] // diff

    def mustUnify(s1: Set[A], s2: Set[A]): Option[(Option[Delta], Set[A], Option[Delta])] =
      if((s1 intersect s2).nonEmpty) Some((diff(s1, s2), s1 union s2, diff(s2, s1)))
      else None

    def canUnify(s1: Set[A], s2: Set[A]): (Option[Delta], Set[A], Option[Delta]) =
      (diff(s1, s2), s1 union s2, diff(s2, s1))

    def dom: Aux[Set[A], Update, Delta] = ???

    @inline private def diff(s1: Set[A], s2: Set[A]): Option[Set[A]] = {
      val d = s1 diff s2
      if(d.nonEmpty) Some(d) else None
    }
  }
  private implicit val idToOption: MonadPartialOrder[Option, Id] = new MonadPartialOrder[Option, Id] {
    implicit val MG: Monad[Option] = implicitly
    implicit val MF: Monad[Id] = implicitly

    def promote[A](m2: Id[A]): Option[A] = Some(m2)
  }
  private implicit def setUnificationByNonEmptyIntersectionOpt[A]: Unification[Option, Set[A]] =
    setUnificationByNonEmptyIntersection[A].promote[Option]
  private implicit def siteDomUnification: Unification[Option, Site.Dom] = Unification.obligatoryPromiseUnification

  implicit def siteUnification: Unification[Option, (Site.Dom, Set[Site.Ref])] = Unification.tuple2[Option, Site.Dom, Set[Site.Ref]]
  implicit def siteWithStateUnification: Unification[Option, SiteWithState] = Unification.tuple2[Option, (Site.Dom, Set[Site.Ref]), SiteState]

  def apply(mods: (SiteLabel, SiteState)*): Option[AdmissibleProteinModifications] = {
    val mods1 = mods.map({ case (s, st) => SiteWithState(s, st) })
    AutoUnificationBag[Option, SiteWithState](mods1:_*).map(AdmissibleProteinModifications(_))
  }

  def noModifications: AdmissibleProteinModifications =
    AdmissibleProteinModifications(AutoUnificationBag.empty[Option, SiteWithState])
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
        if(res == d) None else Some((res, ()))
      }

      override def combineDeltas(d1: Delta, d2: Delta): Delta = ()
    }
}
