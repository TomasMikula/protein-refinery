package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Antichain
import nutcracker.Promise.Completed
import nutcracker.util.ContU
import proteinrefinery.util.syntax._
import proteinrefinery.util.OnceTrigger

import scalaz.Monad
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


  trait Search[M[_]] {
    self: PositiveInfluenceOnRule.Search[M] with
          PositiveInfluenceOnPhosphorylation.Search[M] =>

    implicit def Propagation: nutcracker.Propagation[M]
    implicit def Tracking: proteinrefinery.util.Tracking[M]

    def PhosphorylationSearch: Phosphorylation.Search[M]

    def positiveInfluenceOnStateC(agent: Protein, target: ProteinPattern)(implicit M: Monad[M]): ContU[M, Ref] =
      ContU.sequence(searchByRule(agent, target), searchByPhosphorylation(agent, target))

    private def searchByRule(agent: Protein, target: ProteinPattern)(implicit M: Monad[M]): ContU[M, Ref] = {
      val ap = AgentsPattern.empty.addAgent(target)._1
      for {
        rRef <- Nuggets.rulesC(r => if (r enables ap) OnceTrigger.Fire(()) else OnceTrigger.Sleep())
        r <- rRef.asCont[M]
        inflRef <- positiveInfluenceOnRuleC(agent, r)
        infl <- inflRef.asCont[M]
        res <- Antichain.cellC[M, PositiveInfluenceOnState](ByRule(infl, target))
      } yield res
    }

    private def searchByPhosphorylation(agent: Protein, target: ProteinPattern)(implicit M: Monad[M]): ContU[M, Ref] = {
      val conts = target.mods.mods.list.iterator.mapFilter({ case SiteWithState(site, state) =>
        if (state === SiteState("p")) // XXX hardcoded phosphorylated state as "p"
          site.content match {
            case Completed(label) => Some(label)
            case _ => None
          }
        else
          None
      }).map[ContU[M, Ref]](siteLabel =>
        for {
          k <- Nuggets.kinasesOfC(target.protein, siteLabel)
          phRef <- PhosphorylationSearch.phosphorylationC(k, target.protein, siteLabel)
          ph <- phRef.asCont[M]
          inflRef <- positiveInfluenceOnPhosphorylationC(agent, ph)
          infl <- inflRef.asCont[M]
          res <- Antichain.cellC[M, PositiveInfluenceOnState](byPhosphorylation(infl, target))
        } yield res
      ).toList
      ContU.sequence(conts)
    }

  }

}