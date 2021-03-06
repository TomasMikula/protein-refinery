package proteinrefinery.lib

import nutcracker.data.{Discrete, IncSet}
import nutcracker.util.{ContU, DeepShowK, EqualK, MonadObjectOutput}
import nutcracker.util.EqualK._
import nutcracker.util.ops.tell._
import proteinrefinery.util.OnceTrigger

import scalaz.Monad
import scalaz.syntax.equal._

sealed trait PositiveInfluenceOnRule[Ref[_]] {
  def agent: ProteinPattern[Ref]
  def rule: Rule.Ref[Ref]
}

object PositiveInfluenceOnRule {
  type Ref[Var[_]] = Var[Discrete[PositiveInfluenceOnRule[Var]]]

  // Constructors

  final case class InLhs[Var[_]](agent: ProteinPattern[Var], rule: Rule.Ref[Var]) extends PositiveInfluenceOnRule[Var]
  final case class Indirect[Var[_]](agent: ProteinPattern[Var], ruleInfluence: PositiveInfluenceOfRuleOnRule[Var]) extends PositiveInfluenceOnRule[Var] {
    def rule = ruleInfluence.target
  }

  implicit val deepShowK: DeepShowK[PositiveInfluenceOnRule] = new DeepShowK[PositiveInfluenceOnRule] {
    def show[Ptr[_], M[_]](a: PositiveInfluenceOnRule[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      a match {
        case InLhs(agent, rule) => tell"${M.nest(M(agent))} occurs in LHS of ${M.nest(M.writeObject(rule))}"
        case Indirect(agent, infl) => tell"${M.nest(M(agent))} occurs in LHS of ${M.nest(M(infl))}"
      }
  }


  trait Search[M[_], Var[_], Val[_]] { self: PositiveInfluenceOfRuleOnRule.Search[M, Var, Val] =>

    protected implicit def Propagation: nutcracker.Propagation[M, Var, Val]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var, Val]
    implicit def IncSets: nutcracker.data.IncSets[M, Var, Val]

    def Nuggets: proteinrefinery.lib.Nuggets[M, Var, Val]

    def positiveInfluenceOnRule(agent: Protein, rule: Rule.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[PositiveInfluenceOnRule[Var]]]] =
      IncSets.collect(positiveInfluenceOnRuleC(agent, rule))

    def positiveInfluenceOnRule(agent: ProteinPattern[Var], rule: Rule.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[PositiveInfluenceOnRule[Var]]]] =
      IncSets.collect(positiveInfluenceOnRuleC(agent, rule))

    def positiveInfluenceOnRuleC(agent: ProteinPattern[Var], rule: Rule.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, PositiveInfluenceOnRule[Var]] = {
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
      positiveInfluenceOnRuleC0(agent, test, rule)
    }

    def positiveInfluenceOnRuleC(agent: Protein, r: Rule.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, PositiveInfluenceOnRule[Var]] = {
      positiveInfluenceOnRuleC0(ProteinPattern(agent), (ag1, ag2) => ag1.protein === ag2.protein, r)
    }

    private def positiveInfluenceOnRuleC0(
      agent: ProteinPattern[Var],
      test: (ProteinPattern[Var], ProteinPattern[Var]) => Boolean,
      rr: Rule.Ref[Var]
//      avoid: List[Rule[Var]]
    )(implicit
      M: Monad[M],
      E: EqualK[Var]
    ): ContU[M, PositiveInfluenceOnRule[Var]] = {
      inLhs(agent, test) flatMap { q =>
        if(q === rr) ContU.point(InLhs(agent, rr))
        else positiveInfluenceOfRuleOnRule(q, rr) map { Indirect(agent, _) }
      }
//      val inLhs: ContU[M, PositiveInfluenceOnRule[Var]] =
//        rr.asCont[M] flatMap { r =>
//          if (r.lhs.agentIterator.exists(test(agent, _))) ContU.point(InLhs(agent, r))
//          else ContU.noop
//        }
//
//      val indirect: ContU[M, Ref[Var]] = Nuggets.rulesC(q => // TODO: penalize indirect influence
//        if (avoid.contains(q)) OnceTrigger.Discard()
//        else if (q enables r) OnceTrigger.Fire(())
//        else OnceTrigger.Sleep()
//      ).flatMap(qRef => qRef.asCont[M].flatMap(q => Discrete.map(positiveInfluenceOnRuleC0(agent, test, q, q :: avoid))(posInfl => Indirect(posInfl, r))))
//
//      inLhs match {
//        case Some(inLhs) => ContU.sequence(Discrete.cellC[M, Var, PositiveInfluenceOnRule[Var]](inLhs), indirect)
//        case None => indirect
//      }
    }

    /** Rules where the agent appears in LHS, as determined by the given test. */
    private def inLhs(agent: ProteinPattern[Var], test: (ProteinPattern[Var], ProteinPattern[Var]) => Boolean): ContU[M, Rule.Ref[Var]] =
      Nuggets.rulesC(r =>
        if(r.lhs.agentIterator.exists(test(agent, _))) OnceTrigger.Fire(())
        else OnceTrigger.Sleep()
      )
  }

}