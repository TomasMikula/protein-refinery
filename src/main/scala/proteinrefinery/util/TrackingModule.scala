package proteinrefinery.util

import nutcracker.util.{FreeK, Inject, StateInterpreter}
import nutcracker.toolkit.{ListModule, Module, PersistentStateModule, StashModule}
import scalaz.Lens

trait TrackingModule[Ref[_[_], _], Val[_[_], _]] extends Module {
  def freeTracking[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Tracking[FreeK[F, ?], Ref[FreeK[F, ?], ?], Val[FreeK[F, ?], ?]]
  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S]
}

object TrackingModule {
  def instance[Ref[_[_], _], Val[_[_], _]]: PersistentTrackingModule[Ref, Val] = new TrackingModuleImpl[Ref, Val]
}

trait PersistentTrackingModule[Ref[_[_], _], Val[_[_], _]] extends TrackingModule[Ref, Val] with PersistentStateModule { self =>
  override def stashable: StashTrackingModule[Ref, Val] { type Lang[K[_], A] = self.Lang[K, A] }
}
object PersistentTrackingModule {
  type Aux[Ref[_[_], _], Val[_[_], _], Lang0[_[_], _], State0[_[_]]] = PersistentTrackingModule[Ref, Val] {
    type Lang[K[_], A] = Lang0[K, A]
    type StateK[K[_]] = State0[K]
  }
}

trait StashTrackingModule[Ref[_[_], _], Val[_[_], _]] extends TrackingModule[Ref, Val] with StashModule

private[util] class TrackingListModule[Ref[_[_], _], Val[_[_], _], Lang0[_[_], _], State0[_[_]]](base: PersistentTrackingModule.Aux[Ref, Val, Lang0, State0])
extends ListModule[Lang0, State0](base) with StashTrackingModule[Ref, Val] {

  def freeTracking[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Tracking[FreeK[F, ?], Ref[FreeK[F, ?], ?], Val[FreeK[F, ?], ?]] =
    base.freeTracking[F]

  override def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] =
    base.interpreter[K, S](Lens.nelHeadLens[State0[K]].compose(lens))
}