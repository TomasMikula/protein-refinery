package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Antichain, IncSet}
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContU
import proteinrefinery.util.OnceTrigger

import scalaz.Monad
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


  trait Search[M[_]] {
    implicit def Propagation: nutcracker.Propagation[M]
    implicit def Tracking: proteinrefinery.util.Tracking[M]

    def Nuggets: proteinrefinery.lib.Nuggets[M]

    def positiveInfluenceOnRule(agent: Protein, rule: Rule)(implicit M: Monad[M]): M[IncSetRef[Ref]] =
      IncSet.collect(positiveInfluenceOnRuleC(agent, rule))

    def positiveInfluenceOnRule(agent: ProteinPattern, rule: Rule)(implicit M: Monad[M]): M[IncSetRef[Ref]] =
      IncSet.collect(positiveInfluenceOnRuleC(agent, rule))

    // TODO: make `rule: Rule.Ref`
    def positiveInfluenceOnRuleC(agent: Protein, rule: Rule)(implicit M: Monad[M]): ContU[M, Ref] =
      positiveInfluenceOnRuleC(agent, rule, List(rule))

    def positiveInfluenceOnRuleC(agent: ProteinPattern, rule: Rule)(implicit M: Monad[M]): ContU[M, Ref] = {
      val test = (ag1: ProteinPattern, ag2: ProteinPattern) => {
        ag1.protein === ag2.protein && (
          if (ag1.mods.mods.list.nonEmpty)
            ag1.mods.mods.list.exists(ss1 => {
              val ss2 = ag2.mods.mods.list.find(ss2 => ss1.site === ss2.site)
              ss2.fold(false)(ss1.state === _.state)
            })
          else
            true
          )
      }
      positiveInfluenceOnRuleC0(agent, test, rule, List(rule))
    }

    // TODO: make `avoid: List[Rule.Ref]`
    private def positiveInfluenceOnRuleC(agent: Protein, r: Rule, avoid: List[Rule])(implicit M: Monad[M]): ContU[M, Ref] = {
      positiveInfluenceOnRuleC0(ProteinPattern(agent), (ag1, ag2) => ag1.protein === ag2.protein, r, avoid)
    }

    private def positiveInfluenceOnRuleC0(
      agent: ProteinPattern,
      test: (ProteinPattern, ProteinPattern) => Boolean,
      r: Rule,
      avoid: List[Rule]
    )(implicit
      M: Monad[M]
    ): ContU[M, Ref] = {
      val inLhs: Option[PositiveInfluenceOnRule] =
        if (r.lhs.agentIterator.exists(test(agent, _))) Some(InLhs(agent, r))
        else None

      val indirect: ContU[M, Ref] = Nuggets.rulesC(q => // TODO: penalize indirect influence
        if (avoid.contains(q)) OnceTrigger.Discard()
        else if (q enables r) OnceTrigger.Fire(())
        else OnceTrigger.Sleep()
      ).flatMap(qRef => qRef.asCont[M].flatMap(q => Antichain.map(positiveInfluenceOnRuleC0(agent, test, q, q :: avoid))(posInfl => Indirect(posInfl, r))))

      inLhs match {
        case Some(inLhs) => ContU.sequence(Antichain.cellC[M, PositiveInfluenceOnRule](inLhs), indirect)
        case None => indirect
      }
    }

  }

}