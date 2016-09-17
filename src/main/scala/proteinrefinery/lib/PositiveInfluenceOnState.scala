package proteinrefinery.lib

import nutcracker.Antichain
import nutcracker.util.ContF
import proteinrefinery.DSL
import proteinrefinery.util.syntax._
import proteinrefinery.util.OnceTrigger

sealed trait PositiveInfluenceOnState {
  def agent: Protein
  def target: AdmissibleProteinPattern
}

object PositiveInfluenceOnState {

  type Ref = Antichain.Ref[PositiveInfluenceOnState]

  // Constructors

  final case class ByRule(influenceOnEnablingRule: PositiveInfluenceOnRule, target: AdmissibleProteinPattern) extends PositiveInfluenceOnState {
    def agent = influenceOnEnablingRule.agent
  }

  final case class ByPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: AdmissibleProteinPattern) extends PositiveInfluenceOnState {
    def agent: Protein = infl.agent
  }
  def byPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: AdmissibleProteinPattern): PositiveInfluenceOnState = ByPhosphorylation(infl, target)


  // Search

  def searchC(agent: Protein, target: AdmissibleProteinPattern): ContF[DSL, Ref] =
    ContF.sequence(searchByRule(agent, target), searchByPhosphorylation(agent, target))

  private def searchByRule(agent: Protein, target: AdmissibleProteinPattern): ContF[DSL, Ref] = {
    val ap = AgentsPattern.empty.addAgent(target)._1
    for {
      rRef <- Nuggets.rulesC[DSL](r => if(r enables ap) OnceTrigger.Fire(()) else OnceTrigger.Sleep())
      r <- rRef.asCont[DSL]
      inflRef <- PositiveInfluenceOnRule.searchC(agent, r)
      infl <- inflRef.asCont[DSL]
      res <- Antichain.cellC[DSL, PositiveInfluenceOnState](ByRule(infl, target))
    } yield res
  }

  private def searchByPhosphorylation(agent: Protein, target: AdmissibleProteinPattern): ContF[DSL, Ref] = {
    val conts = target.mods.finalSiteMods.mods.iterator.mapFilter({ case (site, (state, _)) =>
      if (state.label == "p") Some(site) // XXX hardcoded phosphorylated state as "p"
      else None
    }).map[ContF[DSL, Ref]](site =>
      for {
        k <- Nuggets.kinasesOfC[DSL](target.protein, site)
        phRef <- Phosphorylation.searchC(k, target.protein, site)
        ph <- phRef.asCont[DSL]
        inflRef <- PositiveInfluenceOnPhosphorylation.searchC(agent, ph)
        infl <- inflRef.asCont[DSL]
        res <- Antichain.cellC[DSL, PositiveInfluenceOnState](byPhosphorylation(infl, target))
      } yield res
    ).toList
    ContF.sequence(conts)
  }
}