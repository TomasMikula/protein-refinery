package proteinrefinery.lib

import nutcracker.Promise
import proteinrefinery.util.{HomSet, Unification}

import scalaz.Equal
import scalaz.syntax.equal._

case class StateLabel(value: String) extends AnyVal {
  override def toString = value
}

object StateLabel {
  implicit def equalInstance: Equal[StateLabel] = new Equal[StateLabel] {
    def equal(s1: StateLabel, a2: StateLabel): Boolean = s1.value == a2.value
  }
}

object SiteState {
  type SiteState = Promise[StateLabel]
  type Update = Promise.Update[StateLabel]
  type Delta = Promise.Delta[StateLabel]

  def apply(label: String): SiteState = Promise.completed(StateLabel(label))

  def meet(s1: SiteState, s2: SiteState): SiteState =
    Promise.meet(s1, s2)

  implicit def homSet: HomSet.Aux[SiteState, List[Unit]] = new HomSet[SiteState] {
    type HomSet = List[Unit]

    def homSet(s1: SiteState, s2: SiteState): HomSet =
      if (s1 === s2) List(())
      else Nil
  }

  implicit def unificationInstance: Unification.Aux[SiteState, Update, Delta] =
    Unification.promiseUnification[StateLabel]
}