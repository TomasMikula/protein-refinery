package proteinrefinery.ui

import nutcracker.{DRef, Dom, IncSet}
import nutcracker.util.{FreeK, FunctorKA, InjectK}

import scala.language.higherKinds
import scalaz.{Show, ~>}

sealed abstract class UIUpdateLang[K[_], A]

object UIUpdateLang {
  final case class InitGoal      [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], desc: String) extends UIUpdateLang[K, Unit]
  final case class AddSolution   [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A, dom: Dom[A], show: Show[A]) extends UIUpdateLang[K, Unit]
  final case class UpdateSolution[K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A, dom: Dom[A], show: Show[A]) extends UIUpdateLang[K, Unit]
  final case class NewFact[K[_], A](t: FactType[A], fact: A, show: Show[A]) extends UIUpdateLang[K, Unit]

  def initGoal      [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], desc: String): UIUpdateLang[K, Unit] = InitGoal(t, goalRef, desc)
  def addSolution   [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit dom: Dom[A], show: Show[A]): UIUpdateLang[K, Unit] = AddSolution(t, goalRef, solRef, sol, dom, show)
  def updateSolution[K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit dom: Dom[A], show: Show[A]): UIUpdateLang[K, Unit] = UpdateSolution(t, goalRef, solRef, sol, dom, show)
  def newFact[K[_], A](t: FactType[A], fact: A)(implicit A: Show[A]): UIUpdateLang[K, Unit] = NewFact(t, fact, A)

  def initGoalF[A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], desc: String): FreeK[UIUpdateLang, Unit] =
    FreeK.liftF[UIUpdateLang, Unit](initGoal(t, goalRef, desc))
  def addSolutionF[F[_[_], _], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit i: InjectK[UIUpdateLang, F], dom: Dom[A], show: Show[A]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang, F, Unit](addSolution(t, goalRef, solRef, sol))
  def updateSolutionF[F[_[_], _], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit i: InjectK[UIUpdateLang, F], dom: Dom[A], show: Show[A]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang, F, Unit](updateSolution(t, goalRef, solRef, sol))
  def newFactF[A](t: FactType[A], fact: A)(implicit A: Show[A]): FreeK[UIUpdateLang, Unit] =
    FreeK.liftF[UIUpdateLang, Unit](newFact(t, fact))

  implicit def functorKAInstance: FunctorKA[UIUpdateLang] = new FunctorKA[UIUpdateLang] {
    def transform[K[_], L[_], A](u: UIUpdateLang[K, A])(f: K ~> L): UIUpdateLang[L, A] = u match {
      case InitGoal(t, ref, desc) => InitGoal(t, ref, desc)
      case AddSolution(t, ref, sref, s, dom, show) => AddSolution(t, ref, sref, s, dom, show)
      case UpdateSolution(t, ref, sref, s, dom, show) => UpdateSolution(t, ref, sref, s, dom, show)
      case NewFact(t, fact, show) => NewFact(t, fact, show)
    }
  }
}
