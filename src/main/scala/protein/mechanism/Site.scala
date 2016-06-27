package protein.mechanism

import algebra.Eq

import scalaz.Show

case class Site(name: String) {
  override def toString = name
}

object Site {
  implicit def eqInstance: Eq[Site] = new Eq[Site] {
    def eqv(x: Site, y: Site): Boolean = x.name == y.name
  }

  implicit def showInstance: Show[Site] = new Show[Site] {
    override def shows(s: Site): String = s.toString
  }
}