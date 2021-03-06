package proteinrefinery.lib

import nutcracker.data.{Discrete, IncSet}
import nutcracker.ops._
import nutcracker.util.{ContU, DeepEqualK, DeepShow, DeepShowK, EqualK, FreeObjectOutput, IsEqual, MonadObjectOutput, ShowK}
import nutcracker.util.EqualK._
import proteinrefinery.Cost

import scalaz.{Monad, Show}
import scalaz.std.list._
import scalaz.syntax.equal._
import scalaz.syntax.foldable._

case class Assoc[Ref[_]](bindings: List[Binding[Ref]]) extends AnyVal {
  override def toString = show[FreeObjectOutput[String, Ref, ?]].showShallow(ShowK.fromToString)

  def show[M[_]](implicit M: MonadObjectOutput[M, String, Ref]): M[Unit] =
    DeepShow.join(bindings.map(M(_)))(" ; ")
}

object Assoc {

  type Ref[Var[_]] = Var[Discrete[Assoc[Var]]]

  trait Search[M[_], Var[_], Val[_]] {
    implicit def Defer: nutcracker.Defer[M, Cost]
    protected implicit def Propagation: nutcracker.Propagation[M, Var, Val]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var, Val]
    implicit def IncSets: nutcracker.data.IncSets[M, Var, Val]

    def Nuggets: proteinrefinery.lib.Nuggets[M, Var, Val]

    def assoc(p: Protein, q: Protein)(implicit M: Monad[M], ev: EqualK[Var]): M[Var[IncSet[Var[Discrete[Assoc[Var]]]]]] =
      IncSets.collect(assocC(p, q))

    def assocC(p: Protein, q: Protein)(implicit M: Monad[M], ev: EqualK[Var]): ContU[M, Var[Discrete[Assoc[Var]]]] =
      assocC0(Nil, p, q, Nil)

    private def assocC0(leftTail: List[Binding[Var]], p: Protein, q: Protein, rightTail: List[Binding[Var]])(implicit M: Monad[M], ev: EqualK[Var]): ContU[M, Var[Discrete[Assoc[Var]]]] =
      for {
        b <- Nuggets.bindingsOfC(p)
        br <- b.witness.asCont_
        aref <- {
          if (leftTail.nonEmpty && b.leftS === leftTail.head.rightS) ContU.noop[M, Var[Discrete[Assoc[Var]]]] // linter:ignore DuplicateIfBranches
          else if (leftTail.any(_ === b) || rightTail.any(_ === b)) ContU.noop[M, Var[Discrete[Assoc[Var]]]]
          else {
            val indirect0 = assocC0(b :: leftTail, b.getRight(br), q, rightTail)
            val indirect = Defer.deferC(Cost.complexity(10), indirect0)
            if (b.getRight(br) === q) {
              val direct = ContU.liftM(Propagation.newCell(Discrete(Assoc(leftTail reverse_::: b :: rightTail))))
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

  implicit val deepShowKInstance: DeepShowK[Assoc] = new DeepShowK[Assoc] {
    def show[Ptr[_], M[_]](a: Assoc[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      a.show[M]
  }

  implicit def showInstance[Var[_]]: Show[Assoc[Var]] = new Show[Assoc[Var]] {
    override def shows(a: Assoc[Var]): String = a.toString
  }
}