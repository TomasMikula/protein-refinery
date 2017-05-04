package proteinrefinery.lib

import nutcracker.Dom
import nutcracker.util.{DeepEqualK, EqualK, IsEqual}
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{Identification, Unification}

import scalaz.{Equal, \&/}
import scalaz.Isomorphism._
import scalaz.syntax.equal._

case class SiteWithState[Ref[_]](site: ISite[Ref], state: SiteState) {
  def tuple: (ISite[Ref], SiteState) = (site, state)
}
object SiteWithState {
  type Update[Ref[_]] = ISite.Update[Ref] \&/ SiteState.Update
  type Delta[Ref[_]] = ISite.Delta[Ref] \&/ SiteState.Delta

  def apply[Ref[_]](s: SiteLabel, st: SiteState): SiteWithState[Ref] =
    SiteWithState(ISite[Ref](s), st)

  implicit def equalInstance[Ref[_]](implicit ev: EqualK[Ref]): Equal[SiteWithState[Ref]] = new Equal[SiteWithState[Ref]] {
    def equal(a1: SiteWithState[Ref], a2: SiteWithState[Ref]): Boolean =
      (a1.site === a2.site) && (a1.state === a2.state)
  }

  implicit val deepEqualKInstance: DeepEqualK[SiteWithState, SiteWithState] =
    new DeepEqualK[SiteWithState, SiteWithState] {
      def equal[Ptr1[_], Ptr2[_]](f1: SiteWithState[Ptr1], f2: SiteWithState[Ptr2]): IsEqual[Ptr1, Ptr2] =
        IsEqual(f1.site, f2. site) && IsEqual(f1.state, f2. state)
    }

  implicit def unificationInstance[Ref[_]]: Unification.Aux[SiteWithState[Ref], Update[Ref], Delta[Ref]] = {
    implicit def stateUnif = SiteState.unificationInstance

    Unification.tuple2[ISite[Ref], SiteState].translate(pairIso.flip)
  }

  implicit def identificationInstance[Ref[_]]: Identification.Aux[SiteWithState[Ref], Update[Ref], Delta[Ref]] =
    ISite.identificationInstance.zoomOut[SiteWithState[Ref]](_.site)(unificationInstance)

  implicit def domInstance[Ref[_]]: Dom.Aux[SiteWithState[Ref], Update[Ref], Delta[Ref]] =
    unificationInstance[Ref].dom

  private def pairIso[Ref[_]]: SiteWithState[Ref] <=> (ISite[Ref], SiteState) =
    new (SiteWithState[Ref] <=> (ISite[Ref], SiteState)) {
      val to: (SiteWithState[Ref]) => (ISite[Ref], SiteState) = _.tuple
      def from: ((ISite[Ref], SiteState)) => SiteWithState[Ref] = ss => SiteWithState(ss._1, ss._2)
    }
}
