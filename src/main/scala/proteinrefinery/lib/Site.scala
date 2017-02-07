package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Dom.Status
import nutcracker.{Dom, Promise}
import nutcracker.Promise.{Complete, Completed, Conflict, Empty}
import nutcracker.util.{DeepEqualK, DeepShowK, EqualK, FreeObjectOutput, IsEqual, MonadObjectOutput, ShowK}
import proteinrefinery.lib.ProteinModifications.{DefiniteLabel, LocalSiteId, SiteRef}
import proteinrefinery.util.{HomSet, Identification, Unification}
import proteinrefinery.util.HomSet.{Morphisms, Terminal, TerminalOr}

import scalaz.Isomorphism.<=>
import scalaz.{Equal, Show, \&/}
import scalaz.syntax.equal._
import scalaz.syntax.show._

object Site {

  type Dom = Promise[SiteLabel]
  type Ref[Var[_]] = Var[Promise[SiteLabel]]
  type Update = Promise.Update[SiteLabel]
  type Delta = Promise.Delta[SiteLabel]

  type Definite = SiteLabel

  def unknown: Site.Dom = Promise.empty
  def fromLabel(label: SiteLabel): Site.Dom = Promise.completed(label)
  def wrap(site: Definite): Site.Dom = fromLabel(site)

  implicit def showInstance: Show[Site.Dom] = new Show[Site.Dom] {
    override def shows(s: Site.Dom): String = s match {
      case Promise.Completed(t) => t.name
      case Promise.Empty => "?"
      case Promise.Conflict => "⊥"
    }
  }

  implicit def homSet: HomSet.Aux[Site.Dom, TerminalOr[List[Site.Update]]] = new HomSet[Site.Dom] {
    type HomSet = TerminalOr[List[Site.Update]]

    def homSet(s1: Site.Dom, s2: Site.Dom): HomSet = (s1, s2) match {
      case (_, Conflict) => Terminal
      case (Conflict, _) => Morphisms(Nil)
      case (Completed(a), Completed(b)) => if (a == b) Morphisms(List(Nil)) else Morphisms(Nil)
      case (Empty, Empty) => Morphisms(List(Nil))
      case (Empty, Completed(a)) => Morphisms(List(List(Complete(a))))
      case (Completed(_), Empty) => Morphisms(Nil)
    }
  }

  implicit def unificationInstance: Unification.Aux[Site.Dom, Update, Delta] =
    Unification.promiseUnification[SiteLabel]

  implicit def identificationInstance: Identification.Aux[Site.Dom, Update, Delta] =
    Identification.promiseIdentification[SiteLabel]
}

/** Site together with a bag of site references. */
case class ISite[Ref[_]] private(content: Site.Dom, refs: Set[Site.Ref[Ref]]) {

  def show[M[_]](implicit M: MonadObjectOutput[M, String, Ref]): M[Unit] =
    content match {
      case Completed(label) => M.write(label.shows)
      case Empty => refs.headOption match {
        case Some(ref) => M.writeObject(ref)
        case None => M.write("???")
      }
      case Conflict => M.write("⊥")
    }

}

object ISite {
  type Update[Ref[_]] = Site.Update \&/ Set[Site.Ref[Ref]]
  type Delta[Ref[_]]  = Site.Delta  \&/ Set[Site.Ref[Ref]]

  def apply[Ref[_]](site: Site.Definite, refs: Site.Ref[Ref]*): ISite[Ref] = new ISite(Site.wrap(site), Set(refs:_*))
  def apply[Ref[_]](ref: Site.Ref[Ref], refs: Site.Ref[Ref]*): ISite[Ref] = ISite(Site.unknown, Set(refs:_*) + ref)
  def apply[Ref[_]](s: LocalSiteId[Ref]): ISite[Ref] = s match {
    case DefiniteLabel(s) => ISite(s)
    case SiteRef(ref) => ISite(ref)
  }

  implicit def equalInstance[Ref[_]](implicit ev: EqualK[Ref]): Equal[ISite[Ref]] = new Equal[ISite[Ref]] {
    import EqualK._
    def equal(a1: ISite[Ref], a2: ISite[Ref]): Boolean =
      a1.content === a2.content && a1.refs === a2.refs
  }

  implicit val deepEqualKInstance: DeepEqualK[ISite, ISite] = new DeepEqualK[ISite, ISite] {
    def equal[Ptr1[_], Ptr2[_]](s1: ISite[Ptr1], s2: ISite[Ptr2]): IsEqual[Ptr1, Ptr2] =
      IsEqual(s1.content, s2.content) // ignore refs, their content is eventually reflected in the content
  }

  implicit def showInstance[Ref[_]](implicit ev: ShowK[Ref]): Show[ISite[Ref]] = new Show[ISite[Ref]] {
    override def shows(s: ISite[Ref]): String = s.show[FreeObjectOutput[String, Ref, ?]].showShallow(ev)
  }

  implicit val deepShowKInstance: DeepShowK[ISite] = new DeepShowK[ISite] {
    def show[Ptr[_], M[_]](s: ISite[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      s.show[M]
  }

  implicit def identificationInstance[Ref[_]]: Identification.Aux[ISite[Ref], Update[Ref], Delta[Ref]] = {

    implicit def rawIdentificationInstance: Identification.Aux[(Site.Dom, Set[Site.Ref[Ref]]), Update[Ref], Delta[Ref]] = {
      implicit def siteIdentification = Site.identificationInstance

      implicit def setIdentificationByNonEmptyIntersection[A]: Identification.Aux[Set[A], Set[A], Set[A]] = new Identification[Set[A]] {
        type Update = Set[A] // what to add
        type Delta = Set[A] // diff

        def necessarilySame(s1: Set[A], s2: Set[A]): Boolean =
          (s1 intersect s2).nonEmpty

        val unification: Unification.Aux[Set[A], Set[A], Set[A]] = new Unification[Set[A]] {
          type Update = Set[A] // what to add
          type Delta = Set[A] // diff

          def unify(s1: Set[A], s2: Set[A]): (Option[Delta], Set[A], Option[Delta]) =
            (diff(s1, s2), s1 union s2, diff(s2, s1))

          val dom: Dom.Aux[Set[A], Update, Delta] = new Dom[Set[A]] {
            type Update = Set[A] // what to add
            type Delta = Set[A] // diff

            def update(d: Set[A], u: Update): Option[(Set[A], Delta)] = {
              val res = d union u
              if(res.size == d.size) None
              else Some((res, res diff d))
            }

            def combineDeltas(d1: Delta, d2: Delta): Delta = d1 union d2

            def assess(d: Set[A]): Status[Update] = Dom.Refined
          }

          @inline private def diff(s1: Set[A], s2: Set[A]): Option[Set[A]] = {
            val d = s1 diff s2
            if (d.nonEmpty) Some(d) else None
          }
        }
      }

      Identification.tuple2[Site.Dom, Set[Site.Ref[Ref]]]
    }

    Identification.via[ISite[Ref], (Site.Dom, Set[Site.Ref[Ref]])](pairIso)
  }

  // XXX slow (n^2)
  private implicit def setEqual[A: Equal]: Equal[Set[A]] = new Equal[Set[A]] {
    def equal(s1: Set[A], s2: Set[A]): Boolean = s1.size == s2.size && s1.forall(a1 => s2.exists(a2 => a1 === a2))
  }

  // Not really an isomorphism, since ISite does not allow both components to be bottom at the same time.
  // Anyway, it still is a monomorphism, which is sufficient to get a correct Unification instance via.
  private def pairIso[Ref[_]]: ISite[Ref] <=> (Site.Dom, Set[Site.Ref[Ref]]) =
    new (ISite[Ref] <=> (Site.Dom, Set[Site.Ref[Ref]])) {
      def to: (ISite[Ref]) => (Site.Dom, Set[Site.Ref[Ref]]) = is => (is.content, is.refs)

      def from: ((Site.Dom, Set[Site.Ref[Ref]])) => ISite[Ref] = sr => {
        val (s, refs) = sr
        if(s.isEmpty && refs.isEmpty)
            sys.error("Oops, should have never happened to get underspecified site with no refs")
          else
            new ISite(s, refs)
      }
  }
}