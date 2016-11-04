package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker._
import nutcracker.Promise.Completed
import nutcracker.util.ContU
import proteinrefinery.util.syntax._
import proteinrefinery.util.OnceTrigger

import scalaz.Monad
import scalaz.syntax.equal._

sealed trait PositiveInfluenceOnState[Ref[_]] {
  def agent: ProteinPattern[Ref]
  def target: ProteinPattern[Ref]
}

object PositiveInfluenceOnState {

  type Ref[Var[_]] = Var[Antichain[PositiveInfluenceOnState[Var]]]

  // Constructors

  final case class ByRule[Var[_]](influenceOnEnablingRule: PositiveInfluenceOnRule[Var], target: ProteinPattern[Var]) extends PositiveInfluenceOnState[Var] {
    def agent = influenceOnEnablingRule.agent
  }

  final case class ByPhosphorylation[Var[_]](infl: PositiveInfluenceOnPhosphorylation[Var], target: ProteinPattern[Var]) extends PositiveInfluenceOnState[Var] {
    def agent = infl.agent
  }
  def byPhosphorylation[Var[_]](infl: PositiveInfluenceOnPhosphorylation[Var], target: ProteinPattern[Var]): PositiveInfluenceOnState[Var] = ByPhosphorylation(infl, target)


  trait Search[M[_], Var[_]] {
    self: PositiveInfluenceOnRule.Search[M, Var] with
          PositiveInfluenceOnPhosphorylation.Search[M, Var] =>

    implicit def Propagation: nutcracker.Propagation[M, Var]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var]

    def PhosphorylationSearch: Phosphorylation.Search[M, Var]

    def positiveInfluenceOnStateC(agent: Protein, target: ProteinPattern[Var])(implicit M: Monad[M]): ContU[M, Ref[Var]] =
      ContU.sequence(searchByRule(agent, target), searchByPhosphorylation(agent, target))

    private def searchByRule(agent: Protein, target: ProteinPattern[Var])(implicit M: Monad[M]): ContU[M, Ref[Var]] = {
      val ap = AgentsPattern.empty.addAgent(target)._1
      for {
        rRef <- Nuggets.rulesC(r => if (r enables ap) OnceTrigger.Fire(()) else OnceTrigger.Sleep())
        r <- rRef.asCont[M]
        inflRef <- positiveInfluenceOnRuleC(agent, r)
        infl <- inflRef.asCont[M]
        res <- Antichain.cellC[M, Var, PositiveInfluenceOnState[Var]](ByRule(infl, target))
      } yield res
    }

    private def searchByPhosphorylation(agent: Protein, target: ProteinPattern[Var])(implicit M: Monad[M]): ContU[M, Ref[Var]] = {
      val conts = target.mods.mods.list.iterator.mapFilter({ case SiteWithState(site, state) =>
        if (state === SiteState("p")) // XXX hardcoded phosphorylated state as "p"
          site.content match {
            case Completed(label) => Some(label)
            case _ => None
          }
        else
          None
      }).map[ContU[M, Ref[Var]]](siteLabel =>
        for {
          k <- Nuggets.kinasesOfC(target.protein, siteLabel)
          phRef <- PhosphorylationSearch.phosphorylationC(k, target.protein, siteLabel)
          ph <- phRef.asCont[M]
          inflRef <- positiveInfluenceOnPhosphorylationC(agent, ph)
          infl <- inflRef.asCont[M]
          res <- Antichain.cellC[M, Var, PositiveInfluenceOnState[Var]](byPhosphorylation(infl, target))
        } yield res
      ).toList
      ContU.sequence(conts)
    }

  }

}