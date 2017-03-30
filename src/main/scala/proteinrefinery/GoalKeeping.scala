package proteinrefinery

import nutcracker.util.typealigned.APair
import nutcracker.{ListModule, Module, PersistentStateModule, StashModule}
import nutcracker.util.{DeepShow, FreeK, InjectK, Lst, Step, StepT, WriterState}
import scalaz.Id._
import scalaz.Monad

trait GoalKeeping[M[_], Ref[_]] {
  def keep[A](ref: Ref[A])(implicit ev: DeepShow[A, Ref]): M[Unit]
  def list: M[List[APair[Ref, DeepShow[?, Ref]]]]
}

object GoalKeeping {
  def module[Ref[_]]: PersistentGoalKeepingModule[Ref] = new GoalKeepingModuleImpl[Ref]
}

trait GoalKeepingModule[Ref[_]] extends Module {
  def freeGoalKeeping[F[_[_], _]](implicit i: InjectK[Lang, F]): GoalKeeping[FreeK[F, ?], Ref]
  def interpreter: Step[Lang, State]
}

trait PersistentGoalKeepingModule[Ref[_]] extends GoalKeepingModule[Ref] with PersistentStateModule { self =>
  override def stashable: GoalKeepingStashModule[Ref] { type Lang[K[_], A] = self.Lang[K, A] }
}

object PersistentGoalKeepingModule {
  type Aux[Ref[_], Lang0[_[_], _], State0[_[_]]] = PersistentGoalKeepingModule[Ref] {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

trait GoalKeepingStashModule[Ref[_]] extends GoalKeepingModule[Ref] with StashModule

object GoalKeepingStashModule {
  type Aux[State0[_[_]], Ref[_]] = GoalKeepingStashModule[Ref] { type State[K[_]] = State0[K] }
}

class GoalKeepingListModule[Ref[_], Lang0[_[_], _], State0[_[_]]](base: PersistentGoalKeepingModule.Aux[Ref, Lang0, State0])
extends ListModule[Lang0, State0](base) with GoalKeepingStashModule[Ref] {
  override def freeGoalKeeping[F[_[_], _]](implicit i: InjectK[Lang, F]) = base.freeGoalKeeping[F]
  override def interpreter: Step[Lang, State] = base.interpreter.inHead
}

private[proteinrefinery] class GoalKeepingModuleImpl[Ref[_]] extends PersistentGoalKeepingModule[Ref] {
  import GoalKeepingModuleImpl._

  type Lang[K[_], A] = GoalKeepingLang[Ref, K, A]
  type State[K[_]] = GoalKeeper[Ref, K]

  def empty[K[_]]: State[K] = GoalKeeper(Nil)

  final def freeGoalKeeping[F[_[_], _]](implicit i: InjectK[Lang, F]): GoalKeeping[FreeK[F, ?], Ref] = new GoalKeeping[FreeK[F, ?], Ref] {
    def keep[A](ref: Ref[A])(implicit ev: DeepShow[A, Ref]): FreeK[F, Unit] = FreeK.injLiftF[Lang, F, Unit](KeepGoal(ref, ev))
    def list: FreeK[F, List[APair[Ref, DeepShow[?, Ref]]]] = FreeK.injLiftF[Lang, F, List[APair[Ref, DeepShow[?, Ref]]]](ListGoals())
  }

  def interpreter: Step[Lang, State] = new StepT[Id, Lang, State] {
    def apply[K[_]: Monad, A](ga: GoalKeepingLang[Ref, K, A]): WriterState[Lst[K[Unit]], GoalKeeper[Ref, K], A] = ga match {
      case KeepGoal(ref, ev) => WriterState(s => (Lst.empty, s.addGoal(ref)(ev), ()))
      case ListGoals() => WriterState(s => (Lst.empty, s, s.goals))
    }
  }

  override def stashable = new GoalKeepingListModule[Ref, Lang, State](this)
}

private[proteinrefinery] object GoalKeepingModuleImpl {
  private[GoalKeepingModuleImpl] sealed trait GoalKeepingLang[Ref[_], K[_], A]
  private[GoalKeepingModuleImpl] case class KeepGoal[Ref[_], K[_], A](ref: Ref[A], ev: DeepShow[A, Ref]) extends GoalKeepingLang[Ref, K, Unit]
  private[GoalKeepingModuleImpl] case class ListGoals[Ref[_], K[_]]() extends GoalKeepingLang[Ref, K, List[APair[Ref, DeepShow[?, Ref]]]]

  private[GoalKeepingModuleImpl] case class GoalKeeper[Ref[_], K[_]](goals: List[APair[Ref, DeepShow[?, Ref]]]) {

    def addGoal[A](ref: Ref[A])(implicit ev: DeepShow[A, Ref]): GoalKeeper[Ref, K] =
      GoalKeeper(APair.of[Ref, DeepShow[?, Ref]](ref, ev) :: goals)

  }
}