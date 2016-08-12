package proteinrefinery.lib

import nutcracker.IncSet
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.util.{Antichain, OnceTrigger}
import proteinrefinery.{DSL, DSL2, Prg}

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

  def search(agent: Protein, rule: Rule): Prg[IncSetRef[PositiveInfluenceOnRule]] =
    IncSet.collect(searchC(agent, rule))

  def searchC(agent: Protein, rule: Rule): ContF[DSL, PositiveInfluenceOnRule] =
    searchC(agent, rule, List(rule))

  // TODO: make `rule: Rule.Ref`
  def searchC_2(agent: Protein, rule: Rule): ContF[DSL2, Ref] =
    searchC_2(agent, rule, List(rule))

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

  // TODO: make `avoid: List[Rule.Ref]`
  private def searchC_2(agent: Protein, r: Rule, avoid: List[Rule]): ContF[DSL2, Ref] = {
    val inLhs: Option[PositiveInfluenceOnRule] = if(r.lhs.agentIterator.exists(_.protein == agent)) Some(InLhs(agent, r)) else None
    val indirect: ContF[DSL2, Ref] = Nuggets.rulesC[DSL2](q => // TODO: penalize indirect influence
      if(avoid.contains(q)) OnceTrigger.Discard()
      else if(q enables r) OnceTrigger.Fire(())
      else OnceTrigger.Sleep()
    ).flatMap(qRef => qRef.asCont[DSL2].flatMap(q => Antichain.map(searchC_2(agent, q, q :: avoid))(posInfl => Indirect(posInfl, r))))

    inLhs match {
      case Some(inLhs) => ContF.sequence(Antichain.cellC[DSL2, PositiveInfluenceOnRule](inLhs), indirect)
      case None => indirect
    }
  }
}