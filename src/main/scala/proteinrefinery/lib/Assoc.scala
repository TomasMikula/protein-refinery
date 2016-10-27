package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.IncSet._
import nutcracker.util.ContU
import nutcracker.{Antichain, IncSet}
import proteinrefinery.Cost

import scalaz.{Monad, Show}

case class Assoc(bindings: List[Binding]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}

object Assoc {

  trait Search[M[_]] {
    implicit def Defer: nutcracker.Defer[M, Cost]
    implicit def Propagation: nutcracker.Propagation[M]
    implicit def Tracking: proteinrefinery.util.Tracking[M]

    def Nuggets: proteinrefinery.lib.Nuggets[M]

    def assoc(p: Protein, q: Protein)(implicit M: Monad[M]): M[IncSetRef[Antichain.Ref[Assoc]]] =
      IncSet.collect(assocC(p, q))

    def assocC(p: Protein, q: Protein)(implicit M: Monad[M]): ContU[M, Antichain.Ref[Assoc]] =
      assocC0(Nil, p, q, Nil)

    private def assocC0(leftTail: List[Binding], p: Protein, q: Protein, rightTail: List[Binding])(implicit M: Monad[M]): ContU[M, Antichain.Ref[Assoc]] = for {
      bref <- Nuggets.bindingsOfC(p)
      b <- bref.asCont[M]
      aref <- {
        if (leftTail.nonEmpty && b.leftS == leftTail.head.rightS) ContU.noop[M, Antichain.Ref[Assoc]] // linter:ignore DuplicateIfBranches
        else if (leftTail.contains(b) || rightTail.contains(b)) ContU.noop[M, Antichain.Ref[Assoc]]
        else {
          val indirect0 = assocC0(b :: leftTail, b.right, q, rightTail)
          val indirect = Defer.deferC(Cost.complexity(10), indirect0)
          if (b.right == q) {
            val direct = ContU.liftM(Propagation.cell(Antichain(Assoc(leftTail reverse_::: b :: rightTail))))
            ContU.sequence(direct, indirect)
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