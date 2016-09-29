package proteinrefinery.lib

import algebra.Eq

import scalaz.{Equal, Show}

case class Protein(name: Symbol) {
  override def toString = name.name
}

object Protein {
  def apply(name: String): Protein = Protein(Symbol(name))

  implicit def eqInstance: Eq[Protein] = new Eq[Protein] {
    def eqv(x: Protein, y: Protein): Boolean = x.name == y.name
  }

  implicit def equalInstance: Equal[Protein] = new Equal[Protein] {
    def equal(x: Protein, y: Protein): Boolean = x.name == y.name
  }

  implicit def showInstance: Show[Protein] = new Show[Protein] {
    override def shows(p: Protein): String = p.toString
  }
}