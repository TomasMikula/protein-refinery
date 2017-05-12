package proteinrefinery

import nutcracker.toolkit.{ListModule, Module, PersistentStateModule, StashModule}
import nutcracker.util.{DeepShow, FreeK, Inject, Lst, MonadTellState, StateInterpreter, StratifiedMonoidAggregator}
import nutcracker.util.ops._
import nutcracker.util.typealigned.APair
import scalaz.{Bind, Lens, Monoid}

trait GoalKeeping[M[_], Ref[_]] {
  def keep[A](ref: Ref[A])(implicit ev: DeepShow[A, Ref]): M[Unit]
  def list: M[List[APair[Ref, DeepShow[?, Ref]]]]
}

object GoalKeeping {
  def module[Ref[_[_], _]]: PersistentGoalKeepingModule[Ref] = new GoalKeepingModuleImpl[Ref]
}

trait GoalKeepingModule[RefK[_[_], _]] extends Module {
  def freeGoalKeeping[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): GoalKeeping[FreeK[F, ?], RefK[FreeK[F, ?], ?]]
  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S]
}

trait PersistentGoalKeepingModule[Ref[_[_], _]] extends GoalKeepingModule[Ref] with PersistentStateModule { self =>
  override def stashable: GoalKeepingStashModule[Ref] { type Lang[K[_], A] = self.Lang[K, A] }
}

object PersistentGoalKeepingModule {
  type Aux[Ref[_[_], _], Lang0[_[_], _], State0[_[_]]] = PersistentGoalKeepingModule[Ref] {
    type Lang[K[_], A] = Lang0[K, A]
    type StateK[K[_]] = State0[K]
  }
}

trait GoalKeepingStashModule[Ref[_[_], _]] extends GoalKeepingModule[Ref] with StashModule

object GoalKeepingStashModule {
  type Aux[State0[_[_]], Ref[_[_], _]] = GoalKeepingStashModule[Ref] { type StateK[K[_]] = State0[K] }
}

class GoalKeepingListModule[Ref[_[_], _], Lang0[_[_], _], State0[_[_]]](base: PersistentGoalKeepingModule.Aux[Ref, Lang0, State0])
extends ListModule[Lang0, State0](base) with GoalKeepingStashModule[Ref] {
  override def freeGoalKeeping[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]) = base.freeGoalKeeping[F]
  override def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] =
    base.interpreter[K, S](Lens.nelHeadLens[State0[K]].compose(lens))
}

private[proteinrefinery] class GoalKeepingModuleImpl[Ref[_[_], _]] extends PersistentGoalKeepingModule[Ref] {
  import GoalKeepingModuleImpl._

  type Lang[K[_], A] = GoalKeepingLang[Ref[K, ?], K, A]
  type StateK[K[_]] = GoalKeeper[Ref[K, ?], K]

  def emptyK[K[_]]: StateK[K] = GoalKeeper(Nil)

  final def freeGoalKeeping[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): GoalKeeping[FreeK[F, ?], Ref[FreeK[F, ?], ?]] =
    new GoalKeeping[FreeK[F, ?], Ref[FreeK[F, ?], ?]] {
      type K[A] = FreeK[F, A]
      type Ref1[A] = Ref[K, A]

      def keep[A](ref: Ref1[A])(implicit ev: DeepShow[A, Ref1]): FreeK[F, Unit] =
        FreeK.liftF(i(KeepGoal[Ref1, K, A](ref, ev)))

      def list: FreeK[F, List[APair[Ref[FreeK[F, ?], ?], DeepShow[?, Ref[FreeK[F, ?], ?]]]]] =
        FreeK.liftF(i(ListGoals[Ref1, K]()))
    }

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] =
    new StateInterpreter[K, Lang[K, ?], S] {
      def apply[M[_], W, A](ga: GoalKeepingLang[Ref[K, ?], K, A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[GoalKeepingLang[Ref[K, ?], K, ?], K], K: Bind[K]): M[A] =
        go[Ref[K, ?], M, W, A](ga)

      // https://github.com/scala/bug/issues/10292
      private def go[Ref0[_], M[_], W, A](ga: GoalKeepingLang[Ref0, K, A])(implicit lens: Lens[S, GoalKeeper[Ref0, K]], M: MonadTellState[M, W, S], W: Monoid[W]): M[A] =
        ga match {
          case KeepGoal(ref, ev) => M.writerState(s => (W.zero, s set lens.get(s).addGoal(ref)(ev), ()))
          case ListGoals() => M.writerState(s => (W.zero, s, lens.get(s).goals))
        }
    }

  override def stashable = new GoalKeepingListModule[Ref, Lang, StateK](this)
}

private[proteinrefinery] object GoalKeepingModuleImpl {
  private[GoalKeepingModuleImpl] sealed trait GoalKeepingLang[Ref[_], K[_], A]
  private[GoalKeepingModuleImpl] case class KeepGoal[Ref[_], K[_], A](ref: Ref[A], ev: DeepShow[A, Ref]) extends GoalKeepingLang[Ref, K, Unit]
  private[GoalKeepingModuleImpl] case class ListGoals[Ref[_], K[_]]() extends GoalKeepingLang[Ref, K, List[APair[Ref, DeepShow[?, Ref]]]]

  case class GoalKeeper[Ref[_], K[_]](goals: List[APair[Ref, DeepShow[?, Ref]]]) {

    def addGoal[A](ref: Ref[A])(implicit ev: DeepShow[A, Ref]): GoalKeeper[Ref, K] =
      GoalKeeper(APair.of[Ref, DeepShow[?, Ref]](ref, ev) :: goals)

  }
}