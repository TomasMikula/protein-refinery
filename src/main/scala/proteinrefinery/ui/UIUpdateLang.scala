package proteinrefinery.ui

import nutcracker.{DRef, Dom, IncSet}
import nutcracker.util.{FreeK, FunctorKA, InjectK}

import scala.language.higherKinds
import scalaz.{Show, ~>}

sealed abstract class UIUpdateLang[K[_], A]

object UIUpdateLang {
  case class InitGoal      [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], desc: String) extends UIUpdateLang[K, Unit]
  case class AddSolution   [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A, dom: Dom[A], show: Show[A]) extends UIUpdateLang[K, Unit]
  case class UpdateSolution[K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A, dom: Dom[A], show: Show[A]) extends UIUpdateLang[K, Unit]
  case class NewFact[K[_], A](t: FactType[A], fact: A, show: Show[A]) extends UIUpdateLang[K, Unit]

  def initGoal      [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], desc: String): UIUpdateLang[K, Unit] = InitGoal(t, goalRef, desc)
  def addSolution   [K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit dom: Dom[A], show: Show[A]): UIUpdateLang[K, Unit] = AddSolution(t, goalRef, solRef, sol, dom, show)
  def updateSolution[K[_], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit dom: Dom[A], show: Show[A]): UIUpdateLang[K, Unit] = UpdateSolution(t, goalRef, solRef, sol, dom, show)
  def newFact[K[_], A](t: FactType[A], fact: A)(implicit A: Show[A]): UIUpdateLang[K, Unit] = NewFact(t, fact, A)

  def initGoalF[F[_[_], _], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], desc: String)(implicit i: InjectK[UIUpdateLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang, F, Unit](initGoal(t, goalRef, desc))
  def addSolutionF[F[_[_], _], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit i: InjectK[UIUpdateLang, F], dom: Dom[A], show: Show[A]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang, F, Unit](addSolution(t, goalRef, solRef, sol))
  def updateSolutionF[F[_[_], _], A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit i: InjectK[UIUpdateLang, F], dom: Dom[A], show: Show[A]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang, F, Unit](updateSolution(t, goalRef, solRef, sol))
  def newFactF[F[_[_], _], A](t: FactType[A], fact: A)(implicit i: InjectK[UIUpdateLang, F], A: Show[A]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang, F, Unit](newFact(t, fact))

  implicit def functorKAInstance: FunctorKA[UIUpdateLang] = new FunctorKA[UIUpdateLang] {
    def transform[K[_], L[_], A](u: UIUpdateLang[K, A])(f: K ~> L): UIUpdateLang[L, A] = u match {
      case InitGoal(t, ref, desc) => InitGoal(t, ref, desc)
      case AddSolution(t, ref, sref, s, dom, show) => AddSolution(t, ref, sref, s, dom, show)
      case UpdateSolution(t, ref, sref, s, dom, show) => UpdateSolution(t, ref, sref, s, dom, show)
      case NewFact(t, fact, show) => NewFact(t, fact, show)
    }
  }

  implicit def freeUIUpdate[F[_[_], _]](implicit i: InjectK[UIUpdateLang, F]): UIUpdate[FreeK[F, ?], DRef] = new UIUpdate[FreeK[F, ?], DRef] {
    def initGoal[A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], desc: String): FreeK[F, Unit] =
      initGoalF[F, A](t, goalRef, desc)

    def addSolution[A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit dom: Dom[A], show: Show[A]): FreeK[F, Unit] =
      addSolutionF[F, A](t, goalRef, solRef, sol)

    def updateSolution[A](t: GoalType[A], goalRef: DRef[IncSet[DRef[A]]], solRef: DRef[A], sol: A)(implicit dom: Dom[A], show: Show[A]): FreeK[F, Unit] =
      updateSolutionF[F, A](t, goalRef, solRef, sol)

    def newFact[A](t: FactType[A], fact: A)(implicit A: Show[A]): FreeK[F, Unit] =
      newFactF[F, A](t, fact)
  }
}