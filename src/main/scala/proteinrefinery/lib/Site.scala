package proteinrefinery.lib

import algebra.Eq
import nutcracker.Promise

import scalaz.Show

case class Site(name: String) {
  override def toString = name
}

object Site {

  type Dom = Promise[Site]
  type Ref = Promise.Ref[Site]

  type Definite = Site

  def fromLabel(label: Site): Site.Dom = Promise.completed(label)

  implicit def eqInstance: Eq[Site] = new Eq[Site] {
    def eqv(x: Site, y: Site): Boolean = x.name == y.name
  }

  implicit def showInstance: Show[Site] = new Show[Site] {
    override def shows(s: Site): String = s.name
  }

  implicit def domShowInstance: Show[Site.Dom] = new Show[Site.Dom] {
    override def shows(s: Site.Dom): String = s match {
      case Promise.Completed(t) => t.name
      case Promise.Empty => "?"
      case Promise.Conflict => "⊥"
    }
  }
}