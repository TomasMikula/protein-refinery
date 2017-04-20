package proteinrefinery.lib

import nutcracker.Discrete
import nutcracker.util.{ContU, EqualK}
import proteinrefinery.util.OnceTrigger
import scalaz.Monad

sealed trait PositiveInfluenceOnState[Ref[_]] {
  def agent: ProteinPattern[Ref]
  def target: ProteinPattern[Ref]
}

object PositiveInfluenceOnState {

  type Ref[Var[_]] = Var[Discrete[PositiveInfluenceOnState[Var]]]

  // Constructors

  final case class ByRule[Var[_]](influenceOnEnablingRule: PositiveInfluenceOnRule[Var], target: ProteinPattern[Var]) extends PositiveInfluenceOnState[Var] {
    def agent = influenceOnEnablingRule.agent
  }


  trait Search[M[_], Var[_], Val[_]] {
    self: PositiveInfluenceOnRule.Search[M, Var, Val] /*with
          PositiveInfluenceOnPhosphorylation.Search[M, Var]*/ =>

    protected implicit def Propagation: nutcracker.Propagation[M, Var, Val]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var, Val]

    def positiveInfluenceOnStateC(agent: Protein, target: ProteinPattern[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] =
      searchByRule(agent, target)

    private def searchByRule(agent: Protein, target: ProteinPattern[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
      val ap = AgentsPattern.empty.addAgent(target)._1
      for {
        rRef <- Nuggets.rulesC(r => if (r enables ap) OnceTrigger.Fire(()) else OnceTrigger.Sleep())
        infl <- positiveInfluenceOnRuleC(agent, rRef)
        res <- Discrete.cellC[M, Var, Val, PositiveInfluenceOnState[Var]](ByRule(infl, target))
      } yield res
    }

  }

}

trait PositiveInfluenceOnPhosphorylatedStateSearch[M[_], Ref[_], Val[_]] { self: PositiveInfluenceOnState.Search[M, Ref, Val] =>

  def Nuggets: proteinrefinery.lib.Nuggets[M, Ref, Val]

  def positiveInfluenceOnPhosphorylatedStateC(agent: Protein, target: Protein)(implicit M: Monad[M], E: EqualK[Ref]): ContU[M, PositiveInfluenceOnState.Ref[Ref]] =
    Nuggets.phosphoSitesC(target).flatMap(site => {
      val pat = ProteinPattern[Ref](target).addModification(site, SiteState("p")) // XXX hard-coded phosphorylated state as "p"
      positiveInfluenceOnStateC(agent, pat)
    })

}