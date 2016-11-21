package proteinrefinery.lib

import scala.language.higherKinds

import nutcracker.util.{DeepEqual, IsEqual}

import scalaz.{Equal, Semigroup, Show}

sealed trait Protein {
  def isValid: Boolean = this match {
    case ProteinLabel(_) => true
    case Protein.Conflict => false
  }

  final override def toString = this match {
    case ProteinLabel(label) => label.name
    case Protein.Conflict => "⊥"
  }
}

case class ProteinLabel(name: Symbol) extends Protein

object Protein {
  type Delta = Unit

  case object Conflict extends Protein

  def apply(name: String): Protein = Protein(Symbol(name))
  def apply(sym: Symbol): Protein = ProteinLabel(sym)

  def unify(p1: Protein, p2: Protein): (Option[Delta], Protein, Option[Delta]) =
    (p1, p2) match {
      case (ProteinLabel(l1), ProteinLabel(l2)) =>
        if(l1.name == l2.name) (None, p1, None)
        else (Some(()), Conflict, Some(()))
      case (ProteinLabel(_), Conflict) => (Some(()), Conflict, None)
      case (Conflict, ProteinLabel(_)) => (None, Conflict, Some(()))
      case (Conflict, Conflict) => (None, Conflict, None)
    }

  val deltaSemigroup: Semigroup[Delta] = new Semigroup[Delta] {
    def append(f1: Delta, f2: => Delta): Delta = ()
  }

  implicit val equalInstance: Equal[Protein] = new Equal[Protein] {
    def equal(x: Protein, y: Protein): Boolean = (x, y) match {
      case (ProteinLabel(x), ProteinLabel(y)) => x.name == y.name
      case (Conflict, Conflict) => true
      case _ => false
    }
  }

  implicit def deepEqualInstance[Ptr1[_], Ptr2[_]]: DeepEqual[Protein, Protein, Ptr1, Ptr2] =
    new DeepEqual[Protein, Protein, Ptr1, Ptr2] {
      def equal(p1: Protein, p2: Protein): IsEqual[Ptr1, Ptr2] = IsEqual(equalInstance.equal(p1, p2))
    }

  implicit def showInstance: Show[Protein] = new Show[Protein] {
    override def shows(p: Protein): String = p.toString
  }
}