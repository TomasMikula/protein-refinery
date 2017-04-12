package proteinrefinery.util

import nutcracker.util.{FreeK, InjectK, Step}
import nutcracker.{ListModule, Module, PersistentStateModule, StashModule}

trait TrackingModule[Ref[_], Val[_]] extends Module {
  def freeTracking[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], Ref, Val]
  def interpreter: Step[Lang, State]
}

object TrackingModule {
  def instance[Ref[_], Val[_]]: PersistentTrackingModule[Ref, Val] = new TrackingModuleImpl[Ref, Val]
}

trait PersistentTrackingModule[Ref[_], Val[_]] extends TrackingModule[Ref, Val] with PersistentStateModule { self =>
  override def stashable: StashTrackingModule[Ref, Val] { type Lang[K[_], A] = self.Lang[K, A] }
}
object PersistentTrackingModule {
  type Aux[Ref[_], Val[_], Lang0[_[_], _], State0[_[_]]] = PersistentTrackingModule[Ref, Val] {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

trait StashTrackingModule[Ref[_], Val[_]] extends TrackingModule[Ref, Val] with StashModule

private[util] class TrackingListModule[Ref[_], Val[_], Lang0[_[_], _], State0[_[_]]](base: PersistentTrackingModule.Aux[Ref, Val, Lang0, State0])
extends ListModule[Lang0, State0](base) with StashTrackingModule[Ref, Val] {

  def freeTracking[F[_[_], _]](implicit i: InjectK[Lang0, F]): Tracking[FreeK[F, ?], Ref, Val] =
    base.freeTracking[F]

  override def interpreter: Step[Lang, State] =
    base.interpreter.inHead
}