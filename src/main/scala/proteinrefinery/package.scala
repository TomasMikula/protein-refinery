import nutcracker.{DeferModule, Propagation}
import nutcracker.rel.RelModule
import proteinrefinery.util.TrackingModule

package object proteinrefinery extends ImplicitConversions {

  def refinery(): Refinery =
    RefineryImpl(Propagation.module, RelModule.instance)(TrackingModule.instance, DeferModule.instance[Cost])

  def newSession(): RefinerySession =
    newSession(refinery())(GoalKeeping.module)

  def newSession(r: Refinery)(g: GoalKeepingModule[r.Ref]): RefinerySession =
    new RefinerySessionImpl[r.State, g.State, r.Ref](r, g)

  def newReplSession(): ReplSession = {
    val r = stashRefinery
    newReplSession(r)(GoalKeeping.module[r.Ref].stashable)
  }

  def newReplSession(r: StashRefinery)(g: GoalKeepingStashModule[r.Ref]): ReplSession =
    ReplSessionImpl(r)(g)

  private def stashRefinery: StashRefinery = {
    val p = Propagation.module.stashable
    StashRefineryImpl(p, RelModule.instance.stashable)(TrackingModule.instance[p.Ref].stashable, DeferModule.instance[Cost].stashable)
  }
}
