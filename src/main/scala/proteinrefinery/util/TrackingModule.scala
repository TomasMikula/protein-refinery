package proteinrefinery.util

import nutcracker.util.{FreeK, InjectK, Step}
import nutcracker.{ListModule, Module, PersistentStateModule, StashModule}

trait TrackingModule[Ref[_]] extends Module {
  def freeTracking[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], Ref]
  def interpreter: Step[Lang, State]
}

object TrackingModule {
  def instance[Ref[_]]: PersistentTrackingModule[Ref] = new TrackingModuleImpl[Ref]
}

trait PersistentTrackingModule[Ref[_]] extends TrackingModule[Ref] with PersistentStateModule { self =>
  override def stashable: StashTrackingModule[Ref] { type Lang[K[_], A] = self.Lang[K, A] }
}
object PersistentTrackingModule {
  type Aux[Ref[_], Lang0[_[_], _], State0[_[_]]] = PersistentTrackingModule[Ref] {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

trait StashTrackingModule[Ref[_]] extends TrackingModule[Ref] with StashModule

private[util] class TrackingListModule[Ref[_], Lang0[_[_], _], State0[_[_]]](base: PersistentTrackingModule.Aux[Ref, Lang0, State0])
extends ListModule[Lang0, State0](base) with StashTrackingModule[Ref] {

  def freeTracking[F[_[_], _]](implicit i: InjectK[Lang0, F]): Tracking[FreeK[F, ?], Ref] =
    base.freeTracking[F]

  override def interpreter: Step[Lang, State] =
    base.interpreter.inHead
}