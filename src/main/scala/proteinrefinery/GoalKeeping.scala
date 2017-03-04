package proteinrefinery

import nutcracker.{Module, StashModule}
import nutcracker.util.{Exists, FreeK, InjectK, Lst, Step, StepT, WriterState}
import scalaz.Id._

trait GoalKeeping[M[_], Ref[_]] {
  def keep[A](ref: Ref[A]): M[Unit]
  def list: M[List[Exists[Ref]]]
}

object GoalKeeping {
  def module[Ref[_]]: GoalKeepingModule[Ref] = new GoalKeepingModuleImpl[Ref]
}

trait GoalKeepingModule[Ref[_]] extends Module {
  def freeGoalKeeping[F[_[_], _]](implicit i: InjectK[Lang, F]): GoalKeeping[FreeK[F, ?], Ref]
  def interpreter: Step[Lang, State]
}

trait GoalKeepingStashModule[Ref[_]] extends GoalKeepingModule[Ref] with StashModule

object GoalKeepingStashModule {
  type Aux[State0[_[_]], Ref[_]] = GoalKeepingStashModule[Ref] { type State[K[_]] = State0[K] }
}

private[proteinrefinery] class GoalKeepingModuleImpl[Ref[_]] extends GoalKeepingModule[Ref] {
  import GoalKeepingModuleImpl._

  type Lang[K[_], A] = GoalKeepingLang[Ref, K, A]
  type State[K[_]] = GoalKeeper[Ref, K]

  def empty[K[_]]: State[K] = GoalKeeper(Nil)

  final def freeGoalKeeping[F[_[_], _]](implicit i: InjectK[Lang, F]): GoalKeeping[FreeK[F, ?], Ref] = new GoalKeeping[FreeK[F, ?], Ref] {
    def keep[A](ref: Ref[A]): FreeK[F, Unit] = FreeK.injLiftF[Lang, F, Unit](KeepGoal(ref))
    def list: FreeK[F, List[Exists[Ref]]] = FreeK.injLiftF[Lang, F, List[Exists[Ref]]](ListGoals())
  }

  def interpreter: Step[Lang, State] = new StepT[Id, Lang, State] {
    def apply[K[_], A](ga: GoalKeepingLang[Ref, K, A]): WriterState[Lst[K[Unit]], GoalKeeper[Ref, K], A] = ga match {
      case KeepGoal(ref) => WriterState(s => (Lst.empty, s.addGoal(ref), ()))
      case ListGoals() => WriterState(s => (Lst.empty, s, s.goals))
    }
  }
}

private[proteinrefinery] object GoalKeepingModuleImpl {
  private[GoalKeepingModuleImpl] sealed trait GoalKeepingLang[Ref[_], K[_], A]
  private[GoalKeepingModuleImpl] case class KeepGoal[Ref[_], K[_], A](ref: Ref[A]) extends GoalKeepingLang[Ref, K, Unit]
  private[GoalKeepingModuleImpl] case class ListGoals[Ref[_], K[_]]() extends GoalKeepingLang[Ref, K, List[Exists[Ref]]]

  private[GoalKeepingModuleImpl] case class GoalKeeper[Ref[_], K[_]](goals: List[Exists[Ref]]) {
    def addGoal[A](ref: Ref[A]): GoalKeeper[Ref, K] = GoalKeeper(Exists(ref) :: goals)
  }
}