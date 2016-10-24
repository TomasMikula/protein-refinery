package proteinrefinery.lib

import nutcracker.Antichain
import nutcracker.Promise.Completed
import nutcracker.util.ContF
import proteinrefinery.DSL
import proteinrefinery.util.syntax._
import proteinrefinery.util.OnceTrigger

import scalaz.syntax.equal._

sealed trait PositiveInfluenceOnState {
  def agent: ProteinPattern
  def target: ProteinPattern
}

object PositiveInfluenceOnState {

  type Ref = Antichain.Ref[PositiveInfluenceOnState]

  // Constructors

  final case class ByRule(influenceOnEnablingRule: PositiveInfluenceOnRule, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent = influenceOnEnablingRule.agent
  }

  final case class ByPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent = infl.agent
  }
  def byPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: ProteinPattern): PositiveInfluenceOnState = ByPhosphorylation(infl, target)


  // Search

  def searchC(agent: Protein, target: ProteinPattern): ContF[DSL, Ref] =
    ContF.sequence(searchByRule(agent, target), searchByPhosphorylation(agent, target))

  private def searchByRule(agent: Protein, target: ProteinPattern): ContF[DSL, Ref] = {
    val ap = AgentsPattern.empty.addAgent(target)._1
    for {
      rRef <- Nuggets.rulesC[DSL](r => if(r enables ap) OnceTrigger.Fire(()) else OnceTrigger.Sleep())
      r <- rRef.asCont[DSL]
      inflRef <- PositiveInfluenceOnRule.searchC(agent, r)
      infl <- inflRef.asCont[DSL]
      res <- Antichain.cellC[DSL, PositiveInfluenceOnState](ByRule(infl, target))
    } yield res
  }

  private def searchByPhosphorylation(agent: Protein, target: ProteinPattern): ContF[DSL, Ref] = {
    val conts = target.mods.mods.list.iterator.mapFilter({ case SiteWithState(site, state) =>
      if (state === SiteState("p")) // XXX hardcoded phosphorylated state as "p"
        site.content match {
          case Completed(label) => Some(label)
          case _ => None
        }
      else
        None
    }).map[ContF[DSL, Ref]](siteLabel =>
      for {
        k <- Nuggets.kinasesOfC[DSL](target.protein, siteLabel)
        phRef <- Phosphorylation.searchC(k, target.protein, siteLabel)
        ph <- phRef.asCont[DSL]
        inflRef <- PositiveInfluenceOnPhosphorylation.searchC(agent, ph)
        infl <- inflRef.asCont[DSL]
        res <- Antichain.cellC[DSL, PositiveInfluenceOnState](byPhosphorylation(infl, target))
      } yield res
    ).toList
    ContF.sequence(conts)
  }
}