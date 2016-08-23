package proteinrefinery.lib

import nutcracker.util.ContF
import proteinrefinery.DSL
import proteinrefinery.util.{Antichain, OnceTrigger}

sealed trait PositiveInfluenceOnRule {
  def agent: Protein
  def rule: Rule
}

object PositiveInfluenceOnRule {

  type Ref = Antichain.Ref[PositiveInfluenceOnRule]


  // Constructors

  final case class InLhs(agent: Protein, rule: Rule) extends PositiveInfluenceOnRule
  final case class Indirect(influenceOnEnablingRule: PositiveInfluenceOnRule, rule: Rule) extends PositiveInfluenceOnRule {
    def agent = influenceOnEnablingRule.agent
  }


  // Search

  // TODO: make `rule: Rule.Ref`
  def searchC(agent: Protein, rule: Rule): ContF[DSL, Ref] =
    searchC(agent, rule, List(rule))

  // TODO: make `avoid: List[Rule.Ref]`
  private def searchC(agent: Protein, r: Rule, avoid: List[Rule]): ContF[DSL, Ref] = {
    val inLhs: Option[PositiveInfluenceOnRule] = if(r.lhs.agentIterator.exists(_.protein == agent)) Some(InLhs(agent, r)) else None
    val indirect: ContF[DSL, Ref] = Nuggets.rulesC[DSL](q => // TODO: penalize indirect influence
      if(avoid.contains(q)) OnceTrigger.Discard()
      else if(q enables r) OnceTrigger.Fire(())
      else OnceTrigger.Sleep()
    ).flatMap(qRef => qRef.asCont[DSL].flatMap(q => Antichain.map(searchC(agent, q, q :: avoid))(posInfl => Indirect(posInfl, r))))

    inLhs match {
      case Some(inLhs) => ContF.sequence(Antichain.cellC[DSL, PositiveInfluenceOnRule](inLhs), indirect)
      case None => indirect
    }
  }
}