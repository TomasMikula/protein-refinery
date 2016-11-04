package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker._
import nutcracker.util.ContU
import proteinrefinery.Cost

import scalaz.{Monad, Show}

case class Assoc[Ref[_]](bindings: List[Binding[Ref]]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}

object Assoc {

  trait Search[M[_], Ref[_]] {
    implicit def Defer: nutcracker.Defer[M, Cost]
    implicit def Propagation: nutcracker.Propagation[M, Ref]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Ref]
    implicit def IncSets: nutcracker.IncSets[M, Ref]

    def Nuggets: proteinrefinery.lib.Nuggets[M, Ref]

    def assoc(p: Protein, q: Protein)(implicit M: Monad[M]): M[Ref[IncSet[Ref[Antichain[Assoc[Ref]]]]]] =
      IncSets.collect(assocC(p, q))

    def assocC(p: Protein, q: Protein)(implicit M: Monad[M]): ContU[M, Ref[Antichain[Assoc[Ref]]]] =
      assocC0(Nil, p, q, Nil)

    private def assocC0(leftTail: List[Binding[Ref]], p: Protein, q: Protein, rightTail: List[Binding[Ref]])(implicit M: Monad[M]): ContU[M, Ref[Antichain[Assoc[Ref]]]] = for {
      bref <- Nuggets.bindingsOfC(p)
      b <- bref.asCont[M]
      aref <- {
        if (leftTail.nonEmpty && b.leftS == leftTail.head.rightS) ContU.noop[M, Ref[Antichain[Assoc[Ref]]]] // linter:ignore DuplicateIfBranches
        else if (leftTail.contains(b) || rightTail.contains(b)) ContU.noop[M, Ref[Antichain[Assoc[Ref]]]]
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

  implicit def showInstance[Ref[_]]: Show[Assoc[Ref]] = new Show[Assoc[Ref]] {
    override def shows(a: Assoc[Ref]): String = a.toString
  }
}