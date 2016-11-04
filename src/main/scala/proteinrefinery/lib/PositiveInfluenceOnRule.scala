package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker._
import nutcracker.util.{ContU, EqualK}
import nutcracker.util.EqualK._
import proteinrefinery.util.OnceTrigger

import scalaz.Monad
import scalaz.syntax.equal._

sealed trait PositiveInfluenceOnRule[Ref[_]] {
  def agent: ProteinPattern[Ref]
  def rule: Rule[Ref]
}

object PositiveInfluenceOnRule {
  type Ref[Var[_]] = Var[Antichain[PositiveInfluenceOnRule[Var]]]

  // Constructors

  final case class InLhs[Var[_]](agent: ProteinPattern[Var], rule: Rule[Var]) extends PositiveInfluenceOnRule[Var]
  final case class Indirect[Var[_]](influenceOnEnablingRule: PositiveInfluenceOnRule[Var], rule: Rule[Var]) extends PositiveInfluenceOnRule[Var] {
    def agent = influenceOnEnablingRule.agent
  }


  trait Search[M[_], Var[_]] {

    implicit def Propagation: nutcracker.Propagation[M, Var]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var]
    implicit def IncSets: nutcracker.IncSets[M, Var]

    def Nuggets: proteinrefinery.lib.Nuggets[M, Var]

    def positiveInfluenceOnRule(agent: Protein, rule: Rule[Var])(implicit M: Monad[M]): M[Var[IncSet[Ref[Var]]]] =
      IncSets.collect(positiveInfluenceOnRuleC(agent, rule))

    def positiveInfluenceOnRule(agent: ProteinPattern[Var], rule: Rule[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[Ref[Var]]]] =
      IncSets.collect(positiveInfluenceOnRuleC(agent, rule))

    // TODO: make `rule: Rule.Ref`
    def positiveInfluenceOnRuleC(agent: Protein, rule: Rule[Var])(implicit M: Monad[M]): ContU[M, Ref[Var]] =
      positiveInfluenceOnRuleC(agent, rule, List(rule))

    def positiveInfluenceOnRuleC(agent: ProteinPattern[Var], rule: Rule[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
      val test = (ag1: ProteinPattern[Var], ag2: ProteinPattern[Var]) => {
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
    private def positiveInfluenceOnRuleC(agent: Protein, r: Rule[Var], avoid: List[Rule[Var]])(implicit M: Monad[M]): ContU[M, Ref[Var]] = {
      positiveInfluenceOnRuleC0(ProteinPattern(agent), (ag1, ag2) => ag1.protein === ag2.protein, r, avoid)
    }

    private def positiveInfluenceOnRuleC0(
      agent: ProteinPattern[Var],
      test: (ProteinPattern[Var], ProteinPattern[Var]) => Boolean,
      r: Rule[Var],
      avoid: List[Rule[Var]]
    )(implicit
      M: Monad[M]
    ): ContU[M, Ref[Var]] = {
      val inLhs: Option[PositiveInfluenceOnRule[Var]] =
        if (r.lhs.agentIterator.exists(test(agent, _))) Some(InLhs(agent, r))
        else None

      val indirect: ContU[M, Ref[Var]] = Nuggets.rulesC(q => // TODO: penalize indirect influence
        if (avoid.contains(q)) OnceTrigger.Discard()
        else if (q enables r) OnceTrigger.Fire(())
        else OnceTrigger.Sleep()
      ).flatMap(qRef => qRef.asCont[M].flatMap(q => Antichain.map(positiveInfluenceOnRuleC0(agent, test, q, q :: avoid))(posInfl => Indirect(posInfl, r))))

      inLhs match {
        case Some(inLhs) => ContU.sequence(Antichain.cellC[M, Var, PositiveInfluenceOnRule[Var]](inLhs), indirect)
        case None => indirect
      }
    }

  }

}