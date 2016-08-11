package proteinrefinery.lib

import nutcracker.util.ContF
import proteinrefinery.DSL
import proteinrefinery.util.syntax._

sealed trait PositiveInfluenceOnState {
  def agent: Protein
  def target: ProteinPattern
}

object PositiveInfluenceOnState {
  final case class ByRule(influenceOnEnablingRule: PositiveInfluenceOnRule, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent = influenceOnEnablingRule.agent
  }
  final case class ByPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent: Protein = infl.agent
  }

  def searchC(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] =
    ContF.sequence(searchByRule(agent, target), searchByPhosphorylation(agent, target))

  private def searchByRule(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] = {
    val ap = AgentsPattern.empty.addAgent(target)._1
    for {
      r <- ContF.filter(KB.rulesC[DSL])(r => r enables ap)
      infl <- PositiveInfluenceOnRule.searchC(agent, r)
    } yield ByRule(infl, target)
  }

  private def searchByPhosphorylation(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] = {
    val conts = target.mods.mods.iterator.mapFilter({ case (site, state) =>
      if (state.label == "p") Some(site) // XXX
      else None
    }).map[ContF[DSL, PositiveInfluenceOnState]](site =>
      for {
        k <- KB.kinasesOfC[DSL](target.protein, site)
        ph <- Phosphorylation.searchC(k, target.protein, site)
        infl <- PositiveInfluenceOnPhosphorylation.searchC(agent, ph)
      } yield ByPhosphorylation(infl, target)
    ).toList
    ContF.sequence(conts)
  }
}