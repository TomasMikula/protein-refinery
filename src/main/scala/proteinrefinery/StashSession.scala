package proteinrefinery

import nutcracker.util.KPair._
import nutcracker.util.{DeepShow, ShowK}

trait StashSession extends RefinerySession {
  def stash(): Unit
  def restore(): Unit
}

private[proteinrefinery] class StashSessionImpl[Lang[_[_], _], State1[_[_]], State2[_[_]], Ref[_], Val[_]](
  refinery: StashRefinery.Aux[Lang, State1, Ref, Val],
  goalModule: GoalKeepingStashModule.Aux[State2, Ref]
) extends RefinerySessionImpl[State1, State2, Ref, Val](refinery, goalModule) with StashSession {

  private val ev1 = refinery.stashRestore[Prg]
  private val ev2 = goalModule.stashRestore[Prg]

  def stash(): Unit = {
    state = ev1.stash(state._1) :*: ev2.stash(state._2)
  }

  def restore(): Unit = {
    state = ev1.restore(state._1) :*: ev2.restore(state._2)
  }
}

trait ReplSession extends StashSession {
  def listGoals(): Unit
}

private[proteinrefinery] class ReplSessionImpl[Lang[_[_], _], State1[_[_]], State2[_[_]], Ref[_], Val[_]](
  refinery: StashRefinery.Aux[Lang, State1, Ref, Val],
  goalModule: GoalKeepingStashModule.Aux[State2, Ref]
) extends StashSessionImpl[Lang, State1, State2, Ref, Val](refinery, goalModule) with ReplSession {

  def listGoals(): Unit = {
    for((g, i) <- getGoals.reverse.zipWithIndex) {
      printGoal[g.A](g._1, i)(g._2)
    }
  }

  private def printGoal[A](g: Var[A], i: Int)(implicit ev: DeepShow[A, Var]): Unit = {
    println(s"Goal ${i+1}:")
    println(ev.free(deref(g)).printTree(deref, ShowK.fromToString)())
  }
}

object ReplSessionImpl {
  def apply(r: StashRefinery)(g: GoalKeepingStashModule[r.Var]): ReplSessionImpl[r.Lang, r.State, g.State, r.Var, r.Val] =
    new ReplSessionImpl[r.Lang, r.State, g.State, r.Var, r.Val](r, g)
}