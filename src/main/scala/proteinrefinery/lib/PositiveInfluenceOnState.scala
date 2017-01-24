package proteinrefinery.lib

import scala.language.higherKinds

import nutcracker.Antichain
import nutcracker.util.{ContU, EqualK}
import proteinrefinery.util.OnceTrigger

import scalaz.Monad

sealed trait PositiveInfluenceOnState[Ref[_]] {
  def agent: ProteinPattern[Ref]
  def target: ProteinPattern[Ref]
}

object PositiveInfluenceOnState {

  type Ref[Var[_]] = Var[Antichain[PositiveInfluenceOnState[Var]]]

  // Constructors

  final case class ByRule[Var[_]](influenceOnEnablingRule: PositiveInfluenceOnRule[Var], target: ProteinPattern[Var]) extends PositiveInfluenceOnState[Var] {
    def agent = influenceOnEnablingRule.agent
  }


  trait Search[M[_], Var[_]] {
    self: PositiveInfluenceOnRule.Search[M, Var] /*with
          PositiveInfluenceOnPhosphorylation.Search[M, Var]*/ =>

    implicit def Propagation: nutcracker.Propagation[M, Var]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var]

    def positiveInfluenceOnStateC(agent: Protein, target: ProteinPattern[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] =
      searchByRule(agent, target)

    private def searchByRule(agent: Protein, target: ProteinPattern[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
      val ap = AgentsPattern.empty.addAgent(target)._1
      for {
        rRef <- Nuggets.rulesC(r => if (r enables ap) OnceTrigger.Fire(()) else OnceTrigger.Sleep())
        infl <- positiveInfluenceOnRuleC(agent, rRef)
        res <- Antichain.cellC[M, Var, PositiveInfluenceOnState[Var]](ByRule(infl, target))
      } yield res
    }

  }

}