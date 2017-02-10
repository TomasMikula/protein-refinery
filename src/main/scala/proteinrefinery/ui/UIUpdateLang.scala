package proteinrefinery.ui

import nutcracker.{Dom, IncSet}
import nutcracker.util.{FreeK, FunctorKA, InjectK}

import scala.language.higherKinds
import scalaz.{Show, ~>}

sealed abstract class UIUpdateLang[Ref[_], K[_], A] {
  def transform[L[_]](f: K ~> L): UIUpdateLang[Ref, L, A]
}

object UIUpdateLang {
  case class InitGoal[Ref[_], K[_], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], desc: String) extends UIUpdateLang[Ref, K, Unit] {
    def transform[L[_]](f: ~>[K, L]): UIUpdateLang[Ref, L, Unit] = InitGoal(t, goalRef, desc)
  }
  case class AddSolution[Ref[_], K[_], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref], dom: Dom[A[Ref]], show: Show[A[Ref]]) extends UIUpdateLang[Ref, K, Unit] {
    def transform[L[_]](f: ~>[K, L]): UIUpdateLang[Ref, L, Unit] = AddSolution(t, goalRef, solRef, sol, dom, show)
  }
  case class UpdateSolution[Ref[_], K[_], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref], dom: Dom[A[Ref]], show: Show[A[Ref]]) extends UIUpdateLang[Ref, K, Unit] {
    def transform[L[_]](f: ~>[K, L]): UIUpdateLang[Ref, L, Unit] = UpdateSolution(t, goalRef, solRef, sol, dom, show)
  }
  case class NewFact[Ref[_], K[_], A[_[_]]](t: FactType[A], fact: A[Ref], show: Show[A[Ref]]) extends UIUpdateLang[Ref, K, Unit] {
    def transform[L[_]](f: ~>[K, L]): UIUpdateLang[Ref, L, Unit] = NewFact(t, fact, show)
  }

  def initGoal      [Ref[_], K[_], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], desc: String): UIUpdateLang[Ref, K, Unit] = InitGoal(t, goalRef, desc)
  def addSolution   [Ref[_], K[_], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): UIUpdateLang[Ref, K, Unit] = AddSolution(t, goalRef, solRef, sol, dom, show)
  def updateSolution[Ref[_], K[_], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): UIUpdateLang[Ref, K, Unit] = UpdateSolution(t, goalRef, solRef, sol, dom, show)
  def newFact[Ref[_], K[_], A[_[_]]](t: FactType[A], fact: A[Ref])(implicit A: Show[A[Ref]]): UIUpdateLang[Ref, K, Unit] = NewFact(t, fact, A)

  def initGoalF[Ref[_], F[_[_], _], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], desc: String)(implicit i: InjectK[UIUpdateLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang[Ref, ?[_], ?], F, Unit](initGoal(t, goalRef, desc))
  def addSolutionF[Ref[_], F[_[_], _], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit i: InjectK[UIUpdateLang[Ref, ?[_], ?], F], dom: Dom[A[Ref]], show: Show[A[Ref]]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang[Ref, ?[_], ?], F, Unit](addSolution(t, goalRef, solRef, sol))
  def updateSolutionF[Ref[_], F[_[_], _], A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit i: InjectK[UIUpdateLang[Ref, ?[_], ?], F], dom: Dom[A[Ref]], show: Show[A[Ref]]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang[Ref, ?[_], ?], F, Unit](updateSolution(t, goalRef, solRef, sol))
  def newFactF[Ref[_], F[_[_], _], A[_[_]]](t: FactType[A], fact: A[Ref])(implicit i: InjectK[UIUpdateLang[Ref, ?[_], ?], F], A: Show[A[Ref]]): FreeK[F, Unit] =
    FreeK.injLiftF[UIUpdateLang[Ref, ?[_], ?], F, Unit](newFact(t, fact))

  implicit def functorKAInstance[Ref[_]]: FunctorKA[UIUpdateLang[Ref, ?[_], ?]] = new FunctorKA[UIUpdateLang[Ref, ?[_], ?]] {
    def transform[K[_], L[_], A](u: UIUpdateLang[Ref, K, A])(f: K ~> L): UIUpdateLang[Ref, L, A] = u.transform(f)
  }

  implicit def freeUIUpdate[Ref[_], F[_[_], _]](implicit i: InjectK[UIUpdateLang[Ref, ?[_], ?], F]): UIUpdate[FreeK[F, ?], Ref] = new UIUpdate[FreeK[F, ?], Ref] {
    def initGoal[A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], desc: String): FreeK[F, Unit] =
      initGoalF[Ref, F, A](t, goalRef, desc)

    def addSolution[A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): FreeK[F, Unit] =
      addSolutionF[Ref, F, A](t, goalRef, solRef, sol)

    def updateSolution[A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): FreeK[F, Unit] =
      updateSolutionF[Ref, F, A](t, goalRef, solRef, sol)

    def newFact[A[_[_]]](t: FactType[A], fact: A[Ref])(implicit A: Show[A[Ref]]): FreeK[F, Unit] =
      newFactF[Ref, F, A](t, fact)
  }
}