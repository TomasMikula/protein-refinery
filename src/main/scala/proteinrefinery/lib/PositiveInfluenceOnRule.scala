package proteinrefinery.lib

import nutcracker.IncSet
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.{DSL, Prg}

sealed trait PositiveInfluenceOnRule {
  def agent: Protein
  def rule: Rule
}

object PositiveInfluenceOnRule {
  final case class InLhs(agent: Protein, rule: Rule) extends PositiveInfluenceOnRule
  final case class Indirect(influenceOnEnablingRule: PositiveInfluenceOnRule, rule: Rule) extends PositiveInfluenceOnRule {
    def agent = influenceOnEnablingRule.agent
  }

  def search(agent: Protein, rule: Rule): Prg[IncSetRef[PositiveInfluenceOnRule]] =
    IncSet.collect(searchC(agent, rule))

  def searchC(agent: Protein, rule: Rule): ContF[DSL, PositiveInfluenceOnRule] =
    searchC(agent, rule, List(rule))

  private def searchC(agent: Protein, r: Rule, avoid: List[Rule]): ContF[DSL, PositiveInfluenceOnRule] = {
    val inLhs: Option[PositiveInfluenceOnRule] = if(r.lhs.agentIterator.exists(_.protein == agent)) Some(InLhs(agent, r)) else None
    val indirect: ContF[DSL, PositiveInfluenceOnRule] = KB.rulesC[DSL].flatMap(q => { // TODO: penalize
      if(!avoid.contains(q) && (q enables r)) searchC(agent, q, q :: avoid).map(posInfl => Indirect(posInfl, r))
      else ContF.noop[DSL, PositiveInfluenceOnRule]
    })

    inLhs match {
      case Some(inLhs) => ContF.sequence(ContF.point(inLhs), indirect)
      case None => indirect
    }
  }
}