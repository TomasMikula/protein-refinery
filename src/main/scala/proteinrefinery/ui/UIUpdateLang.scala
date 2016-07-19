package proteinrefinery.ui

import nutcracker.{Diff, IncSet}
import nutcracker.IncSet.IncSetRef
import nutcracker.util.{FreeK, FunctorKA, InjectK}

import scala.language.higherKinds
import scalaz.{Show, ~>}

sealed abstract class UIUpdateLang[K[_], A]

object UIUpdateLang {
  final case class TrackGoal[K[_], A](t: GoalType[A], desc: String, ref: IncSetRef[A], initialValue: IncSet[A], show: Show[A]) extends UIUpdateLang[K, Unit]
  final case class GoalUpdated[K[_], A](t: GoalType[A], ref: IncSetRef[A], newValue: IncSet[A], delta: Diff[Set[A]]) extends UIUpdateLang[K, Unit]
  final case class NewFact[K[_], A](t: FactType[A], fact: A, show: Show[A]) extends UIUpdateLang[K, Unit]

  def trackGoal[K[_], A](t: GoalType[A], desc: String, ref: IncSetRef[A], initialValue: IncSet[A])(implicit A: Show[A]): UIUpdateLang[K, Unit] = TrackGoal(t, desc, ref, initialValue, A)
  def goalUpdated[K[_], A](t: GoalType[A], ref: IncSetRef[A], newValue: IncSet[A], delta: Diff[Set[A]]): UIUpdateLang[K, Unit] = GoalUpdated(t, ref, newValue, delta)
  def newFact[K[_], A](t: FactType[A], fact: A)(implicit A: Show[A]): UIUpdateLang[K, Unit] = NewFact(t, fact, A)

  def trackGoalF[A](t: GoalType[A], desc: String, ref: IncSetRef[A], initialValue: IncSet[A])(implicit T: Show[A]): FreeK[UIUpdateLang, Unit] =
    FreeK.liftF[UIUpdateLang, Unit](trackGoal(t, desc, ref, initialValue))
  def goalUpdatedF[A](t: GoalType[A], ref: IncSetRef[A], newValue: IncSet[A], delta: Diff[Set[A]]): FreeK[UIUpdateLang, Unit] =
    FreeK.liftF[UIUpdateLang, Unit](goalUpdated(t, ref, newValue, delta))
  def newFactF[A](t: FactType[A], fact: A)(implicit A: Show[A]): FreeK[UIUpdateLang, Unit] =
    FreeK.liftF[UIUpdateLang, Unit](newFact(t, fact))

  implicit def functorKAInstance: FunctorKA[UIUpdateLang] = new FunctorKA[UIUpdateLang] {
    def transform[K[_], L[_], A](u: UIUpdateLang[K, A])(f: K ~> L): UIUpdateLang[L, A] = u match {
      case TrackGoal(t, desc, ref, initialValue, show) => TrackGoal(t, desc, ref, initialValue, show)
      case GoalUpdated(t, ref, newVal, delta) => GoalUpdated(t, ref, newVal, delta)
      case NewFact(t, fact, show) => NewFact(t, fact, show)
    }
  }
}
