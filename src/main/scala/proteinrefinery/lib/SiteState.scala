package proteinrefinery.lib

import nutcracker.Dom
import nutcracker.Dom.Status
import nutcracker.util.Uninhabited
import proteinrefinery.util.{HomSet, Unification}

case class SiteState(label: String) {
  override def toString = label
}

object SiteState {

  implicit def domInstance: Dom.Aux[SiteState, Uninhabited, Uninhabited] = new Dom[SiteState] {
    type Update = Uninhabited
    type Delta = Uninhabited

    def update(d: SiteState, u: Update): Option[(SiteState, Delta)] = sys.error("unreachable code")

    def combineDeltas(d1: Delta, d2: Delta): Delta = sys.error("unreachable code")

    def assess(d: SiteState): Status[Update] = Dom.Refined
  }

  implicit def homSet: HomSet.Aux[SiteState, List[Unit]] = new HomSet[SiteState] {
    type HomSet = List[Unit]

    def homSet(s1: SiteState, s2: SiteState): HomSet =
      if (s1 == s2) List(())
      else Nil
  }

  implicit def unificationInstance: Unification[Option, SiteState] = new Unification[Option, SiteState] {
    type Update = Uninhabited
    type Delta = Uninhabited

    def mustUnify(s1: SiteState, s2: SiteState): Option[Option[(Option[Delta], SiteState, Option[Delta])]] = {
      // When we consider state within an agent, we never _have_ to unify two states within an agent,
      // because they can refer to different sites. Since there is never an obligation to unify,
      // there can never be failure either.
      Some(None)
      //^    ^
      //|    |
      //|    +---- no obligation to unify
      //+--------- not a failure
    }

    def canUnify(s1: SiteState, s2: SiteState): Option[(Option[Delta], SiteState, Option[Delta])] = {
      if(s1 == s2) Some((None, s1, None))
      else None
    }

    val dom: Dom.Aux[SiteState, Update, Delta] = SiteState.domInstance
  }
}