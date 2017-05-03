import nutcracker.{DeferModule, Propagation}
import nutcracker.rel.RelModule
import proteinrefinery.util.TrackingModule

package object proteinrefinery extends ImplicitConversions {

  def refinery(): Refinery =
    RefineryImpl(Propagation.module, RelModule.instance)(TrackingModule.instance, DeferModule.instance[Cost])

  def newSession(): RefinerySession =
    newSession(refinery())(GoalKeeping.module)

  def newSession(r: Refinery)(g: GoalKeepingModule[r.VarK]): RefinerySession =
    new RefinerySessionImpl[r.StateK, g.StateK, r.VarK, r.ValK](r, g)

  def newReplSession(): ReplSession = {
    val r = stashRefinery
    newReplSession(r)(GoalKeeping.module[r.VarK].stashable)
  }

  def newReplSession(r: StashRefinery)(g: GoalKeepingStashModule[r.VarK]): ReplSession =
    ReplSessionImpl(r)(g)

  private def stashRefinery: StashRefinery = {
    val p = Propagation.module.stashable
    StashRefineryImpl(p, RelModule.instance.stashable)(TrackingModule.instance[p.VarK, p.ValK].stashable, DeferModule.instance[Cost].stashable)
  }
}
