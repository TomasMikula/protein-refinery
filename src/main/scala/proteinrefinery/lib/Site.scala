package proteinrefinery.lib

import nutcracker.Promise
import nutcracker.Promise.{Complete, Completed, Conflict, Empty}
import proteinrefinery.util.HomSet
import proteinrefinery.util.HomSet.{Morphisms, Terminal, TerminalOr}

import scalaz.Show

object Site {

  type Dom = Promise[SiteLabel]
  type Ref = Promise.Ref[SiteLabel]
  type Update = Promise.Complete[SiteLabel]

  type Definite = SiteLabel

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
}
