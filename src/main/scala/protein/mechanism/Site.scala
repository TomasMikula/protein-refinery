package protein.mechanism

import algebra.Eq

case class Site(name: String) {
  override def toString = name
}

object Site {
  implicit def eqInstance: Eq[Site] = new Eq[Site] {
    def eqv(x: Site, y: Site): Boolean = x.name == y.name
  }
}