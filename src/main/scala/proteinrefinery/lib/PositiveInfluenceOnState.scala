package proteinrefinery.lib

import nutcracker.util.ContF
import proteinrefinery.{DSL, DSL2}
import proteinrefinery.util.{Antichain, OnceTrigger}
import proteinrefinery.util.syntax._

sealed trait PositiveInfluenceOnState {
  def agent: Protein
  def target: ProteinPattern
}

object PositiveInfluenceOnState {

  type Ref = Antichain.Ref[PositiveInfluenceOnState]

  // Constructors

  final case class ByRule(influenceOnEnablingRule: PositiveInfluenceOnRule, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent = influenceOnEnablingRule.agent
  }

  final case class ByPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent: Protein = infl.agent
  }
  def byPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: ProteinPattern): PositiveInfluenceOnState = ByPhosphorylation(infl, target)


  // Search

  def searchC(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] =
    ContF.sequence(searchByRule(agent, target), searchByPhosphorylation(agent, target))

  def searchC_2(agent: Protein, target: ProteinPattern): ContF[DSL2, Ref] =
    ContF.sequence(searchByRule_2(agent, target), searchByPhosphorylation_2(agent, target))

  private def searchByRule(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] = {
    val ap = AgentsPattern.empty.addAgent(target)._1
    for {
      r <- ContF.filter(KB.rulesC[DSL])(r => r enables ap)
      infl <- PositiveInfluenceOnRule.searchC(agent, r)
    } yield ByRule(infl, target)
  }

  private def searchByRule_2(agent: Protein, target: ProteinPattern): ContF[DSL2, Ref] = {
    val ap = AgentsPattern.empty.addAgent(target)._1
    for {
      rRef <- Nuggets.rulesC[DSL2](r => if(r enables ap) OnceTrigger.Fire(()) else OnceTrigger.Sleep())
      r <- rRef.asCont[DSL2]
      inflRef <- PositiveInfluenceOnRule.searchC_2(agent, r)
      infl <- inflRef.asCont[DSL2]
      res <- Antichain.cellC[DSL2, PositiveInfluenceOnState](ByRule(infl, target))
    } yield res
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

  private def searchByPhosphorylation_2(agent: Protein, target: ProteinPattern): ContF[DSL2, Ref] = {
    val conts = target.mods.mods.iterator.mapFilter({ case (site, state) =>
      if (state.label == "p") Some(site) // XXX
      else None
    }).map[ContF[DSL2, Ref]](site =>
      for {
        k <- Nuggets.kinasesOfC[DSL2](target.protein, site)
        phRef <- Phosphorylation.searchC_2(k, target.protein, site)
        ph <- phRef.asCont[DSL2]
        inflRef <- PositiveInfluenceOnPhosphorylation.searchC_2(agent, ph)
        infl <- inflRef.asCont[DSL2]
        res <- Antichain.cellC[DSL2, PositiveInfluenceOnState](byPhosphorylation(infl, target))
      } yield res
    ).toList
    ContF.sequence(conts)
  }
}