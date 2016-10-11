package proteinrefinery.lib

import nutcracker.{Dom, Promise}
import nutcracker.Dom.Status
import nutcracker.util.Uninhabited
import proteinrefinery.util.{HomSet, Unification}

import scalaz.Equal
import scalaz.syntax.equal._

case class SiteState(label: String) {
  override def toString = label
}

object SiteState {

  def meet(s1: SiteState, s2: SiteState): Promise[SiteState] =
    if(s1 === s2) Promise.completed(s1)
    else Promise.empty

  implicit def domInstance: Dom.Aux[SiteState, Uninhabited, Uninhabited] = new Dom[SiteState] {
    type Update = Uninhabited
    type Delta = Uninhabited

    def update(d: SiteState, u: Update): Option[(SiteState, Delta)] = sys.error("unreachable code")

    def combineDeltas(d1: Delta, d2: Delta): Delta = sys.error("unreachable code")

    def assess(d: SiteState): Status[Update] = Dom.Refined
  }

  implicit def EqualInstance: Equal[SiteState] = new Equal[SiteState] {
    def equal(s1: SiteState, s2: SiteState): Boolean = s1.label == s2.label
  }

  implicit def homSet: HomSet.Aux[SiteState, List[Unit]] = new HomSet[SiteState] {
    type HomSet = List[Unit]

    def homSet(s1: SiteState, s2: SiteState): HomSet =
      if (s1 == s2) List(())
      else Nil
  }

  implicit def unificationInstance: Unification.Aux[SiteState, Uninhabited, Uninhabited, Option] = new Unification[SiteState] {
    type Update = Uninhabited
    type Delta = Uninhabited
    type F[X] = Option[X]

    def unify(s1: SiteState, s2: SiteState): Option[(Option[Delta], SiteState, Option[Delta])] = {
      if(s1 == s2) Some((None, s1, None))
      else None
    }

    val dom: Dom.Aux[SiteState, Update, Delta] = SiteState.domInstance
  }
}