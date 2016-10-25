package proteinrefinery.lib

import nutcracker.{Antichain, IncSet}
import nutcracker.IncSet.IncSetRef
import nutcracker.util.{ContF, FreeK}
import proteinrefinery.DSL
import proteinrefinery.util.OnceTrigger

import scalaz.syntax.equal._

sealed trait PositiveInfluenceOnRule {
  def agent: ProteinPattern
  def rule: Rule
}

object PositiveInfluenceOnRule {

  type Ref = Antichain.Ref[PositiveInfluenceOnRule]


  // Constructors

  final case class InLhs(agent: ProteinPattern, rule: Rule) extends PositiveInfluenceOnRule
  final case class Indirect(influenceOnEnablingRule: PositiveInfluenceOnRule, rule: Rule) extends PositiveInfluenceOnRule {
    def agent = influenceOnEnablingRule.agent
  }


  // Search

  def search(agent: Protein, rule: Rule): FreeK[DSL, IncSetRef[Ref]] =
    IncSet.collect(searchC(agent, rule))

  def search(agent: ProteinPattern, rule: Rule): FreeK[DSL, IncSetRef[Ref]] =
    IncSet.collect(searchC(agent, rule))

  // TODO: make `rule: Rule.Ref`
  def searchC(agent: Protein, rule: Rule): ContF[DSL, Ref] =
    searchC(agent, rule, List(rule))

  def searchC(agent: ProteinPattern, rule: Rule): ContF[DSL, Ref] = {
    val test = (ag1: ProteinPattern, ag2: ProteinPattern) => {
      ag1.protein === ag2.protein && (
        if(ag1.mods.mods.list.nonEmpty)
          ag1.mods.mods.list.exists(ss1 => {
            val ss2 = ag2.mods.mods.list.find(ss2 => ss1.site === ss2.site)
            ss2.fold(false)(ss1.state === _.state)
          })
        else
          true
      )
    }
    searchC0(agent, test, rule, List(rule))
  }

  // TODO: make `avoid: List[Rule.Ref]`
  private def searchC(agent: Protein, r: Rule, avoid: List[Rule]): ContF[DSL, Ref] = {
    searchC0(ProteinPattern(agent), (ag1, ag2) => ag1.protein === ag2.protein, r, avoid)
  }

  private def searchC0(agent: ProteinPattern, test: (ProteinPattern, ProteinPattern) => Boolean, r: Rule, avoid: List[Rule]): ContF[DSL, Ref] = {
    val inLhs: Option[PositiveInfluenceOnRule] =
      if(r.lhs.agentIterator.exists(test(agent, _))) Some(InLhs(agent, r))
      else None

    val indirect: ContF[DSL, Ref] = Nuggets.rulesC[DSL](q => // TODO: penalize indirect influence
      if(avoid.contains(q)) OnceTrigger.Discard()
      else if(q enables r) OnceTrigger.Fire(())
      else OnceTrigger.Sleep()
    ).flatMap(qRef => qRef.asCont[DSL].flatMap(q => Antichain.map(searchC0(agent, test, q, q :: avoid))(posInfl => Indirect(posInfl, r))))

    inLhs match {
      case Some(inLhs) => ContF.sequence(Antichain.cellC[DSL, PositiveInfluenceOnRule](inLhs), indirect)
      case None => indirect
    }
  }
}