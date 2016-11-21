package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Antichain
import nutcracker.util.{DeepEqualK, EqualK, IsEqual}

import scalaz.{Lens, Monad, Show, State, Store}
import scalaz.syntax.equal._

sealed trait PhosphoTarget[Ref[_]] {
  def witness: Rule[Ref]

  def kinaseIndex: AgentIndex

  def substrateIndex: AgentIndex

  def targetSite: ISite[Ref]


  def kinase: Protein = witness.lhs(kinaseIndex).protein

  def substrate: Protein = witness.lhs(substrateIndex).protein
}

object PhosphoTarget {

  private case class PhosphoTarget0[Var[_]](
    witness: Rule[Var],
    kinaseIndex: AgentIndex,
    substrateIndex: AgentIndex,
    targetSite: ISite[Var]
  ) extends PhosphoTarget[Var]

  def apply[Var[_]](
    witness: Rule[Var],
    kinaseIndex: AgentIndex,
    substrateIndex: AgentIndex,
    targetSite: ISite[Var]
  ): PhosphoTarget[Var] =
    PhosphoTarget0(witness, kinaseIndex, substrateIndex, targetSite)

  def apply[Var[_]](kinase: Protein, substrate: Protein, targetSite: ISite[Var]): PhosphoTarget[Var] = {
    import AgentsPattern._
    (for {
      ki <- addAgent(ProteinPattern[Var](kinase))
      si <- addAgent(ProteinPattern[Var](substrate))
      lhs <- State.get[AgentsPattern[Var]]
      action = Modify[Var](si, ProteinModifications.noModifications, ProteinModifications(targetSite -> SiteState("p")), Some(ki)) // XXX hardcoded phosphorylation as "p"
    } yield PhosphoTarget(Rule(lhs, List(action)), ki, si, targetSite)).eval(AgentsPattern.empty)
  }

  /** Lens to access the rule witnessing the PhosphoTarget.
    * Make sure that you only update the rule with a "compatible" rule, e.g. refined rule, so that
    * the kinase and substrate indices don't become invalid.
    */
  def witness[Var[_]]: Lens[PhosphoTarget[Var], Rule[Var]] =
    Lens(pt => Store(r => PhosphoTarget(r, pt.kinaseIndex, pt.substrateIndex, pt.targetSite), pt.witness))


  type Ref[Var[_]] = Var[Antichain[PhosphoTarget[Var]]]

  implicit def showInstance[Var[_]]: Show[PhosphoTarget[Var]] = new Show[PhosphoTarget[Var]] {
    override def shows(p: PhosphoTarget[Var]): String = p.toString
  }

  implicit def deepEqualKInstance: DeepEqualK[PhosphoTarget, PhosphoTarget] =
    new DeepEqualK[PhosphoTarget, PhosphoTarget] {
      def equal[Ptr1[_], Ptr2[_]](a1: PhosphoTarget[Ptr1], a2: PhosphoTarget[Ptr2]): IsEqual[Ptr1, Ptr2] =
        if(a1.kinaseIndex === a2.kinaseIndex && a1.substrateIndex === a2.substrateIndex)
          IsEqual(a1.targetSite, a2.targetSite) && IsEqual(a1.witness, a2.witness)
        else
          IsEqual(false)
    }

  trait Ops[M[_], Var[_]] {
    def AgentsPatternOps: AgentsPattern.Ops[M, Var]

    def define(kinase: Protein, substrate: Protein, targetSite: ISite[Var])(implicit M: Monad[M], E: EqualK[Var]): M[PhosphoTarget[Var]] = {
      import AgentsPattern._
      import proteinrefinery.util.Identification.Syntax._

      PhosphoTarget(kinase, substrate, targetSite)
      val st = for {
        ki <- addAgent(ProteinPattern[Var](kinase)).lift[M]
        si <- addAgent(ProteinPattern[Var](substrate)).lift[M]
        ai <- AgentsPatternOps.requireAssoc(ki, si, a => !(ISite(a.bindings.last.rightS) necessarilySame targetSite))
        lhs <- State.get[AgentsPattern[Var]].lift[M]
        action = Modify[Var](si, ProteinModifications.noModifications, ProteinModifications(targetSite -> SiteState("p")), Some(ki)) // XXX hardcoded phosphorylation as "p"
      } yield PhosphoTarget(
        Rule(lhs, List(action)),
        ki,
        si,
        targetSite
      )

      st.eval(AgentsPattern.empty)
    }
  }

}