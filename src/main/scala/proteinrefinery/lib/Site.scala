package proteinrefinery.lib

import nutcracker.Promise

import scalaz.Show

object Site {

  type Dom = Promise[SiteLabel]
  type Ref = Promise.Ref[SiteLabel]

  type Definite = SiteLabel

  def fromLabel(label: SiteLabel): Site.Dom = Promise.completed(label)

  implicit def showInstance: Show[Site.Dom] = new Show[Site.Dom] {
    override def shows(s: Site.Dom): String = s match {
      case Promise.Completed(t) => t.name
      case Promise.Empty => "?"
      case Promise.Conflict => "⊥"
    }
  }
}
