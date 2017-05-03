package proteinrefinery

import nutcracker.{StashDeferModule, StashPropagationModule, StashRestore}
import nutcracker.StashRestore._
import nutcracker.rel.StashRelModule
import proteinrefinery.util.StashTrackingModule

trait StashRefinery extends Refinery {
  implicit def stashRestore[K[_]]: StashRestore[StateK[K]]
}

object StashRefinery {
  type Aux[Lang0[_[_], _], State0[_[_]], Ref0[_[_], _], Val0[_[_], _]] = StashRefinery {
    type Lang[K[_], A] = Lang0[K, A]
    type StateK[K[_]] = State0[K]
    type VarK[K[_], A] = Ref0[K, A]
    type ValK[K[_], A] = Val0[K, A]
  }
}

private[proteinrefinery] class StashRefineryImpl[Ref0[_[_], _], Val0[_[_], _], PropState[_[_]], RelState[_[_]], TrckState[_[_]], DeferState[_[_]]](
  propMod: StashPropagationModule { type VarK[K[_], A] = Ref0[K, A]; type ValK[K[_], A] = Val0[K, A]; type StateK[K[_]] = PropState[K] },
  relMod: StashRelModule { type StateK[K[_]] = RelState[K] },
  trckMod: StashTrackingModule[Ref0, Val0] { type StateK[K[_]] = TrckState[K] },
  defMod: StashDeferModule[Cost] { type StateK[K[_]] = DeferState[K] }
) extends RefineryImpl[Ref0, Val0, PropState, RelState, TrckState, DeferState](propMod, relMod, trckMod, defMod) with StashRefinery {

  def stashRestore[K[_]]: StashRestore[StateK[K]] =
    propMod.stashRestore[K] :*: relMod.stashRestore[K] :*: trckMod.stashRestore[K] :*: defMod.stashRestore[K]
}

private[proteinrefinery] object StashRefineryImpl {
  def apply(propMod: StashPropagationModule, relMod: StashRelModule)(trckMod: StashTrackingModule[propMod.VarK, propMod.ValK], defMod: StashDeferModule[Cost]): StashRefineryImpl[propMod.VarK, propMod.ValK, propMod.StateK, relMod.StateK, trckMod.StateK, defMod.StateK] =
    new StashRefineryImpl[propMod.VarK, propMod.ValK, propMod.StateK, relMod.StateK, trckMod.StateK, defMod.StateK](propMod, relMod, trckMod, defMod)
}