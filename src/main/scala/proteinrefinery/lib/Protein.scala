package proteinrefinery.lib

import algebra.Eq

import scalaz.{Equal, Show}

sealed trait Protein {
  final override def toString = this match {
    case ProteinLabel(label) => label.name
    case Protein.Conflict => "⊥"
  }
}

case class ProteinLabel(name: Symbol) extends Protein

object Protein {
  case object Conflict extends Protein

  def apply(name: String): Protein = Protein(Symbol(name))
  def apply(sym: Symbol): Protein = ProteinLabel(sym)

  implicit def eqInstance: Eq[Protein] = new Eq[Protein] {
    def eqv(x: Protein, y: Protein): Boolean = (x, y) match {
      case (ProteinLabel(x), ProteinLabel(y)) => x.name == y.name
      case (Conflict, Conflict) => true
      case _ => false
    }
  }

  implicit def equalInstance: Equal[Protein] = new Equal[Protein] {
    def equal(x: Protein, y: Protein): Boolean = (x, y) match {
      case (ProteinLabel(x), ProteinLabel(y)) => x.name == y.name
      case (Conflict, Conflict) => true
      case _ => false
    }
  }

  implicit def showInstance: Show[Protein] = new Show[Protein] {
    override def shows(p: Protein): String = p.toString
  }
}