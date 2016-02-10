package protein.mechanism

import algebra.Eq

case class Protein(name: Symbol) {
  override def toString = name.name
}

object Protein {
  implicit def eqInstance: Eq[Protein] = new Eq[Protein] {
    def eqv(x: Protein, y: Protein): Boolean = x.name == y.name
  }
}