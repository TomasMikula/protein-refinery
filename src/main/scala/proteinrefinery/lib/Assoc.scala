package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Antichain, IncSet}
import nutcracker.ops._
import nutcracker.util.{ContU, DeepEqualK, EqualK, IsEqual}
import nutcracker.util.EqualK._
import proteinrefinery.Cost

import scalaz.{Monad, Show}
import scalaz.std.list._
import scalaz.syntax.equal._
import scalaz.syntax.foldable._

case class Assoc[Ref[_]](bindings: List[Binding[Ref]]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}

object Assoc {

  type Ref[Var[_]] = Var[Antichain[Assoc[Var]]]

  trait Search[M[_], Var[_]] {
    implicit def Defer: nutcracker.Defer[M, Cost]
    implicit def Propagation: nutcracker.Propagation[M, Var]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var]
    implicit def IncSets: nutcracker.IncSets[M, Var]

    def Nuggets: proteinrefinery.lib.Nuggets[M, Var]

    def assoc(p: Protein, q: Protein)(implicit M: Monad[M], ev: EqualK[Var]): M[Var[IncSet[Var[Antichain[Assoc[Var]]]]]] =
      IncSets.collect(assocC(p, q))

    def assocC(p: Protein, q: Protein)(implicit M: Monad[M], ev: EqualK[Var]): ContU[M, Var[Antichain[Assoc[Var]]]] =
      assocC0(Nil, p, q, Nil)

    private def assocC0(leftTail: List[Binding[Var]], p: Protein, q: Protein, rightTail: List[Binding[Var]])(implicit M: Monad[M], ev: EqualK[Var]): ContU[M, Var[Antichain[Assoc[Var]]]] =
      for {
        b <- Nuggets.bindingsOfC(p)
        br <- b.witness.asCont[M]
        aref <- {
          if (leftTail.nonEmpty && b.leftS === leftTail.head.rightS) ContU.noop[M, Var[Antichain[Assoc[Var]]]] // linter:ignore DuplicateIfBranches
          else if (leftTail.any(_ === b) || rightTail.any(_ === b)) ContU.noop[M, Var[Antichain[Assoc[Var]]]]
          else {
            val indirect0 = assocC0(b :: leftTail, b.getRight(br), q, rightTail)
            val indirect = Defer.deferC(Cost.complexity(10), indirect0)
            if (b.getRight(br) === q) {
              val direct = ContU.liftM(Propagation.cell(Antichain(Assoc(leftTail reverse_::: b :: rightTail))))
              ContU.sequence(direct, indirect)
            } else
              indirect
          }
        }
      } yield aref

  }


  // Typeclass instances

  implicit val deepEqualKInstance: DeepEqualK[Assoc, Assoc] = new DeepEqualK[Assoc, Assoc] {
    def equal[Ptr1[_], Ptr2[_]](a1: Assoc[Ptr1], a2: Assoc[Ptr2]): IsEqual[Ptr1, Ptr2] =
      IsEqual(a1.bindings, a2.bindings)
  }

  implicit def showInstance[Var[_]]: Show[Assoc[Var]] = new Show[Assoc[Var]] {
    override def shows(a: Assoc[Var]): String = a.toString
  }
}