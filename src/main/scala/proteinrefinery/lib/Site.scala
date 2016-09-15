package proteinrefinery.lib

import algebra.Eq
import nutcracker.Antichain

import scalaz.Show

case class Site(name: String) {
  override def toString = name
}

object Site {

  type Dom = Antichain[Site]
  type Ref = Antichain.Ref[Site]

  implicit def eqInstance: Eq[Site] = new Eq[Site] {
    def eqv(x: Site, y: Site): Boolean = x.name == y.name
  }

  implicit def showInstance: Show[Site] = new Show[Site] {
    override def shows(s: Site): String = s.toString
  }

  implicit def domShowInstance: Show[Site.Dom] = new Show[Site.Dom] {
    override def shows(s: Site.Dom): String = s.value.toString
  }
}