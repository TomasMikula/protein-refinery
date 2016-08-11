package proteinrefinery.lib

import nutcracker.util.ContF
import proteinrefinery.DSL

sealed trait PositiveInfluenceOnKinaseActivity {
  def agent: Protein
  def kinase: Protein
}

object PositiveInfluenceOnKinaseActivity {
  final case class PositiveInfluenceOnActiveState(infl: PositiveInfluenceOnState) extends PositiveInfluenceOnKinaseActivity {
    def agent: Protein = infl.agent
    def kinase: Protein = infl.target.protein
  }

  def searchC(agent: Protein, kinase: Protein): ContF[DSL, PositiveInfluenceOnKinaseActivity] =
    KB.kinaseActivityC[DSL](kinase).flatMap(PositiveInfluenceOnState.searchC(agent, _).map(PositiveInfluenceOnActiveState(_)))
}