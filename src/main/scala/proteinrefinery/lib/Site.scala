package proteinrefinery.lib

import nutcracker.{Dom, Promise}
import nutcracker.Promise.{Complete, Completed, Conflict, Empty}
import proteinrefinery.util.{HomSet, Unification}
import proteinrefinery.util.HomSet.{Morphisms, Terminal, TerminalOr}

import scalaz.Show
import scalaz.syntax.equal._

object Site {

  type Dom = Promise[SiteLabel]
  type Ref = Promise.Ref[SiteLabel]
  type Update = Promise.Update[SiteLabel]
  type Delta = Promise.Delta[SiteLabel]

  type Definite = SiteLabel

  def unknown: Site.Dom = Promise.empty
  def fromLabel(label: SiteLabel): Site.Dom = Promise.completed(label)

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

  implicit def unificationInstance: Unification.Aux0[Site.Dom, Option] = new Unification[Site.Dom] {
    type Update = Site.Update
    type Delta = Site.Delta
    type F[X] = Option[X]

    def mustUnify(s1: Site.Dom, s2: Site.Dom): Option[Option[(Option[Delta], Site.Dom, Option[Delta])]] =
      (s1, s2) match {
        case (Completed(t1), Completed(t2)) => if(t1 === t2) Some(Some((None, s1, None))) else Some(None)
        case (Conflict, _) => None // pretend failure to unify, instead unifying with a failure (which is always possible)
        case (_, Conflict) => None // pretend failure to unify, instead unifying with a failure (which is always possible)
        case _ => Some(None)
      }

    def unify(s1: Site.Dom, s2: Site.Dom): Option[(Option[Delta], Site.Dom, Option[Delta])] =
      (s1, s2) match {
        case (Empty, Empty) => Some((None, Empty, None))
        case (Empty, Completed(_)) => Some((Some(()), s2, None))
        case (Completed(_), Empty) => Some((None, s1, Some(())))
        case (Completed(t1), Completed(t2)) => if(t1 === t2) Some((None, s1, None)) else None
        case (Conflict, Conflict) => Some((None, Conflict, None))
        case (Conflict, _) => Some((None, Conflict, Some(())))
        case (_, Conflict) => Some((Some(()), Conflict, None))
      }

    def dom: Dom.Aux[Site.Dom, Update, Delta] = implicitly[Dom.Aux[Site.Dom, Update, Delta]]
  }
}
