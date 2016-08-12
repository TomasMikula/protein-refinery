package proteinrefinery.lib

import nutcracker.util.ContF
import proteinrefinery.{DSL, DSL2}
import proteinrefinery.util.Antichain

sealed trait PositiveInfluenceOnKinaseActivity {
  def agent: Protein
  def kinase: Protein
}

object PositiveInfluenceOnKinaseActivity {

  type Ref = Antichain.Ref[PositiveInfluenceOnKinaseActivity]

  // Constructors

  final case class PositiveInfluenceOnActiveState(infl: PositiveInfluenceOnState) extends PositiveInfluenceOnKinaseActivity {
    def agent: Protein = infl.agent
    def kinase: Protein = infl.target.protein
  }
  def positiveInfluenceOnActiveState(infl: PositiveInfluenceOnState): PositiveInfluenceOnKinaseActivity = PositiveInfluenceOnActiveState(infl)


  // Search

  def searchC(agent: Protein, kinase: Protein): ContF[DSL, PositiveInfluenceOnKinaseActivity] =
    KB.kinaseActivityC[DSL](kinase).flatMap(PositiveInfluenceOnState.searchC(agent, _).map(positiveInfluenceOnActiveState(_)))

  def searchC_2(agent: Protein, kinase: Protein): ContF[DSL2, Ref] = for {
    ppref <- Nuggets.kinaseActivityC[DSL2](kinase)
    res <- Antichain.map(Antichain.mapC(ppref)(pp => PositiveInfluenceOnState.searchC_2(agent, pp)))(positiveInfluenceOnActiveState(_))
  } yield res
}