package proteinrefinery.lib

import nutcracker.{DeferLang, IncSet}
import nutcracker.IncSet._
import nutcracker.util.ContF
import proteinrefinery.{Cost, DSL, Prg}

import scalaz.Show

case class Assoc(bindings: List[Binding]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}

object Assoc {

  // Search

  def search(p: Protein, q: Protein): Prg[IncSetRef[Assoc]] =
    IncSet.collect(searchC(p, q))

  def searchC(p: Protein, q: Protein): ContF[DSL, Assoc] =
    search0(Nil, p, q, Nil)

  private def search0(leftTail: List[Binding], p: Protein, q: Protein, rightTail: List[Binding]): ContF[DSL, Assoc] =
    KB.bindingsOfC[DSL](p) flatMap { b =>
      if(leftTail.nonEmpty && b.leftS == leftTail.head.rightS) ContF.noop // linter:ignore DuplicateIfBranches
      else if(leftTail.contains(b) || rightTail.contains(b)) ContF.noop
      else {
        val indirect0 = search0(b :: leftTail, b.right, q, rightTail)
        val indirect = DeferLang.deferC(Cost.complexity(10), indirect0)
        if(b.right == q) {
          val direct = ContF.point[DSL, Assoc](Assoc(leftTail reverse_::: b :: rightTail))
          ContF.sequence(direct, indirect)
        } else
          indirect
      }
    }


  // Typeclass instances

  implicit def showInstance: Show[Assoc] = new Show[Assoc] {
    override def shows(a: Assoc): String = a.toString
  }
}