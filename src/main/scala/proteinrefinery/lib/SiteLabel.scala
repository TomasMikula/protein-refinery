package proteinrefinery.lib

import algebra.Eq

import scalaz.{Equal, Show}

case class SiteLabel(name: String) {
  override def toString = name
}

object SiteLabel {

  implicit def eqInstance: Eq[SiteLabel] = new Eq[SiteLabel] {
    def eqv(x: SiteLabel, y: SiteLabel): Boolean = x.name == y.name
  }

  implicit def equalInstance: Equal[SiteLabel] = new Equal[SiteLabel] {
    def equal(s1: SiteLabel, s2: SiteLabel): Boolean = s1.name == s2.name
  }

  implicit def showInstance: Show[SiteLabel] = new Show[SiteLabel] {
    override def shows(s: SiteLabel): String = s.name
  }
}

