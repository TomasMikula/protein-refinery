package proteinrefinery

import nutcracker.{StashDeferModule, StashPropagationModule, StashRestore}
import nutcracker.StashRestore._
import nutcracker.rel.StashRelModule
import proteinrefinery.util.StashTrackingModule

trait StashRefinery extends Refinery {
  implicit def stashRestore[K[_]]: StashRestore[State[K]]
}

object StashRefinery {
  type Aux[Lang0[_[_], _], State0[_[_]], Ref0[_]] = StashRefinery {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
    type Ref[A] = Ref0[A]
  }
}

private[proteinrefinery] class StashRefineryImpl[Ref0[_], PropState[_[_]], RelState[_[_]], TrckState[_[_]], DeferState[_[_]]](
  propMod: StashPropagationModule { type Ref[A] = Ref0[A]; type State[K[_]] = PropState[K] },
  relMod: StashRelModule { type State[K[_]] = RelState[K] },
  trckMod: StashTrackingModule[Ref0] { type State[K[_]] = TrckState[K] },
  defMod: StashDeferModule[Cost] { type State[K[_]] = DeferState[K] }
) extends RefineryImpl[Ref0, PropState, RelState, TrckState, DeferState](propMod, relMod, trckMod, defMod) with StashRefinery {

  def stashRestore[K[_]]: StashRestore[State[K]] =
    propMod.stashRestore[K] :*: relMod.stashRestore[K] :*: trckMod.stashRestore[K] :*: defMod.stashRestore[K]
}

private[proteinrefinery] object StashRefineryImpl {
  def apply(propMod: StashPropagationModule, relMod: StashRelModule)(trckMod: StashTrackingModule[propMod.Ref], defMod: StashDeferModule[Cost]): StashRefineryImpl[propMod.Ref, propMod.State, relMod.State, trckMod.State, defMod.State] =
    new StashRefineryImpl[propMod.Ref, propMod.State, relMod.State, trckMod.State, defMod.State](propMod, relMod, trckMod, defMod)
}