package proteinrefinery.lib

import nutcracker.Antichain
import nutcracker.util.ContF
import proteinrefinery.DSL

sealed trait PositiveInfluenceOnKinaseActivity {
  def agent: ProteinPattern
  def kinase: Protein
}

object PositiveInfluenceOnKinaseActivity {

  type Ref = Antichain.Ref[PositiveInfluenceOnKinaseActivity]

  // Constructors

  final case class PositiveInfluenceOnActiveState(infl: PositiveInfluenceOnState) extends PositiveInfluenceOnKinaseActivity {
    def agent = infl.agent
    def kinase: Protein = infl.target.protein
  }
  def positiveInfluenceOnActiveState(infl: PositiveInfluenceOnState): PositiveInfluenceOnKinaseActivity = PositiveInfluenceOnActiveState(infl)


  trait Search { self: PositiveInfluenceOnState.Search =>

    def Nuggets: proteinrefinery.lib.Nuggets

    def positiveInfluenceOnKinaseActivityC(agent: Protein, kinase: Protein): ContF[DSL, Ref] = for {
      ppref <- Nuggets.kinaseActivityC[DSL](kinase)
      res <- Antichain.map(Antichain.mapC(ppref)(pp => positiveInfluenceOnStateC(agent, pp)))(positiveInfluenceOnActiveState(_))
    } yield res

  }

}