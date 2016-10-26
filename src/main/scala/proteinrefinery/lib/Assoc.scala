package proteinrefinery.lib

import nutcracker.IncSet._
import nutcracker.util.ContF
import nutcracker.{Antichain, DeferLang, IncSet, PropagationLang}
import proteinrefinery.{Cost, DSL, Prg}

import scalaz.Show

case class Assoc(bindings: List[Binding]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}

object Assoc {

  trait Search {

    def Nuggets: proteinrefinery.lib.Nuggets

    def assoc(p: Protein, q: Protein): Prg[IncSetRef[Antichain.Ref[Assoc]]] =
      IncSet.collect(assocC(p, q))

    def assocC(p: Protein, q: Protein): ContF[DSL, Antichain.Ref[Assoc]] =
      assocC0(Nil, p, q, Nil)

    private def assocC0(leftTail: List[Binding], p: Protein, q: Protein, rightTail: List[Binding]): ContF[DSL, Antichain.Ref[Assoc]] = for {
      bref <- Nuggets.bindingsOfC[DSL](p)
      b <- bref.asCont[DSL]
      aref <- {
        if (leftTail.nonEmpty && b.leftS == leftTail.head.rightS) ContF.noop[DSL, Antichain.Ref[Assoc]] // linter:ignore DuplicateIfBranches
        else if (leftTail.contains(b) || rightTail.contains(b)) ContF.noop[DSL, Antichain.Ref[Assoc]]
        else {
          val indirect0 = assocC0(b :: leftTail, b.right, q, rightTail)
          val indirect = DeferLang.deferC(Cost.complexity(10), indirect0)
          if (b.right == q) {
            val direct = ContF.liftM(PropagationLang.cellF(Antichain(Assoc(leftTail reverse_::: b :: rightTail))).inject[DSL])
            ContF.sequence(direct, indirect)
          } else
            indirect
        }
      }
    } yield aref

  }


  // Typeclass instances

  implicit def showInstance: Show[Assoc] = new Show[Assoc] {
    override def shows(a: Assoc): String = a.toString
  }
}