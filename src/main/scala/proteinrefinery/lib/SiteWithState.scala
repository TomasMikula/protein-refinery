package proteinrefinery.lib

import nutcracker.Dom
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{Identification, Unification}

import scalaz.{Equal, \&/}
import scalaz.Id._
import scalaz.Isomorphism._
import scalaz.syntax.equal._

case class SiteWithState(site: ISite, state: SiteState) {
  def tuple: (ISite, SiteState) = (site, state)
}
object SiteWithState {
  type Update = ISite.Update \&/ SiteState.Update
  type Delta = ISite.Delta \&/ SiteState.Delta

  def apply(s: SiteLabel, st: SiteState): SiteWithState =
    SiteWithState(ISite(s), st)

  implicit val equalInstance: Equal[SiteWithState] = new Equal[SiteWithState] {
    def equal(a1: SiteWithState, a2: SiteWithState): Boolean =
      (a1.site === a2.site) && (a1.state === a2.state)
  }

  implicit def unificationInstance: Unification.Aux[SiteWithState, Update, Delta, Id] = {
    implicit def stateUnif = SiteState.unificationInstance

    Unification.tuple2[Id, ISite, SiteState].translate(pairIso.flip)
  }

  implicit def identificationInstance: Identification.Aux[SiteWithState, Update, Delta, Id] =
    ISite.identificationInstance.zoomOut[SiteWithState](_.site)(unificationInstance)

  implicit def domInstance: Dom.Aux[SiteWithState, Update, Delta] =
    unificationInstance.dom

  private val pairIso: SiteWithState <=> (ISite, SiteState) = new (SiteWithState <=> (ISite, SiteState)) {
    val to: (SiteWithState) => (ISite, SiteState) = _.tuple
    def from: ((ISite, SiteState)) => SiteWithState = ss => SiteWithState(ss._1, ss._2)
  }
}
