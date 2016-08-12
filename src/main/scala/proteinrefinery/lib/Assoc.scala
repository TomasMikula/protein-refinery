package proteinrefinery.lib

import nutcracker.{DeferLang, IncSet, PropagationLang}
import nutcracker.IncSet._
import nutcracker.util.ContF
import proteinrefinery.util.Antichain
import proteinrefinery.{Cost, DSL, DSL2, Prg, Prg2}

import scalaz.Show

case class Assoc(bindings: List[Binding]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}

object Assoc {

  // Search

  def search(p: Protein, q: Protein): Prg[IncSetRef[Assoc]] =
    IncSet.collect(searchC(p, q))

  def search_2(p: Protein, q: Protein): Prg2[IncSetRef[Antichain.Ref[Assoc]]] =
    IncSet.collect(searchC_2(p, q))

  def searchC(p: Protein, q: Protein): ContF[DSL, Assoc] =
    search0(Nil, p, q, Nil)

  def searchC_2(p: Protein, q: Protein): ContF[DSL2, Antichain.Ref[Assoc]] =
    search0_2(Nil, p, q, Nil)

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

  private def search0_2(leftTail: List[Binding], p: Protein, q: Protein, rightTail: List[Binding]): ContF[DSL2, Antichain.Ref[Assoc]] = for {
    bref <- Nuggets.bindingsOfC[DSL2](p)
    b <- bref.asCont[DSL2]
    aref <- {
      if(leftTail.nonEmpty && b.leftS == leftTail.head.rightS) ContF.noop[DSL2, Antichain.Ref[Assoc]] // linter:ignore DuplicateIfBranches
      else if(leftTail.contains(b) || rightTail.contains(b)) ContF.noop[DSL2, Antichain.Ref[Assoc]]
      else {
        val indirect0 = search0_2(b :: leftTail, b.right, q, rightTail)
        val indirect = DeferLang.deferC(Cost.complexity(10), indirect0)
        if(b.right == q) {
          val direct = ContF.liftM(PropagationLang.cellF(Antichain(Assoc(leftTail reverse_::: b :: rightTail))).inject[DSL2])
          ContF.sequence(direct, indirect)
        } else
          indirect
      }
    }
  } yield aref


  // Typeclass instances

  implicit def showInstance: Show[Assoc] = new Show[Assoc] {
    override def shows(a: Assoc): String = a.toString
  }
}