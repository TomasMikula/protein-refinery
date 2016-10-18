package proteinrefinery.lib

import nutcracker.Dom.Status
import nutcracker.{Dom, Promise}
import nutcracker.Promise.{Complete, Completed, Conflict, Empty}
import proteinrefinery.util.{HomSet, Identification, Unification}
import proteinrefinery.util.HomSet.{Morphisms, Terminal, TerminalOr}

import scalaz.Isomorphism.<=>
import scalaz.{Equal, Show, \&/}
import scalaz.syntax.equal._

object Site {

  type Dom = Promise[SiteLabel]
  type Ref = Promise.Ref[SiteLabel]
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
case class ISite private(content: Site.Dom, refs: Set[Site.Ref])

object ISite {
  type Update = Site.Update \&/ Set[Site.Ref]
  type Delta  = Site.Delta  \&/ Set[Site.Ref]

  def apply(site: Site.Definite, refs: Site.Ref*): ISite = new ISite(Site.wrap(site), Set(refs:_*))
  def apply(ref: Site.Ref, refs: Site.Ref*): ISite = ISite(Site.unknown, Set(refs:_*) + ref)

  implicit def equalInstance: Equal[ISite] = new Equal[ISite] {
    def equal(a1: ISite, a2: ISite): Boolean =
      a1.content === a2.content && a1.refs === a2.refs

    // XXX slow (n^2)
    private implicit def setEqual[A: Equal]: Equal[Set[A]] = new Equal[Set[A]] {
      def equal(s1: Set[A], s2: Set[A]): Boolean = s1.size == s2.size && s1.forall(a1 => s2.exists(a2 => a1 === a2))
    }
  }

  implicit def identificationInstance: Identification.Aux[ISite, Update, Delta] = {

    implicit def rawIdentificationInstance: Identification.Aux[(Site.Dom, Set[Site.Ref]), Update, Delta] = {
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

      Identification.tuple2[Site.Dom, Set[Site.Ref]]
    }

    Identification.via[ISite, (Site.Dom, Set[Site.Ref])](pairIso)
  }

  // Not really an isomorphism, since ISite does not allow both components to be bottom at the same time.
  // Anyway, it still is a monomorphism, which is sufficient to get a correct Unification instance via.
  private val pairIso: ISite <=> (Site.Dom, Set[Site.Ref]) = new (ISite <=> (Site.Dom, Set[Site.Ref])) {
    def to: (ISite) => (Site.Dom, Set[Site.Ref]) = is => (is.content, is.refs)

    def from: ((Site.Dom, Set[Site.Ref])) => ISite = sr => {
      val (s, refs) = sr
      if(s.isEmpty && refs.isEmpty)
          sys.error("Oops, should have never happened to get underspecified site with no refs")
        else
          new ISite(s, refs)
    }
  }
}