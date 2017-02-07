package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Discrete
import nutcracker.util.{DeepEqualK, DeepShowK, EqualK, IsEqual, MonadObjectOutput}
import nutcracker.util.ops._

import scalaz.{Lens, Monad, Show, State, Store}
import scalaz.syntax.equal._
import scalaz.syntax.monad._

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

  def apply[Var[_]](kinase: Protein, substrate: Protein, targetSite: ISite[Var])(implicit ev: EqualK[Var]): PhosphoTarget[Var] = {
    import AgentsPattern._
    (for {
      ki <- addAgent(ProteinPattern[Var](kinase))
      si <- addAgent(ProteinPattern[Var](substrate))
      lhs <- State.get[AgentsPattern[Var]]
      action = Modify[Var](si, ProteinModifications.noModifications, ProteinModifications(targetSite -> SiteState("p")), Some(ki)) // XXX hardcoded phosphorylation as "p"
    } yield PhosphoTarget(Rule(lhs, List(action)), ki, si, targetSite)).eval(AgentsPattern.empty)
  }

  def apply[Var[_]](pt: PhosphoTriple[Var])(implicit ev: EqualK[Var]): PhosphoTarget[Var] =
    apply(pt.kinase, pt.substrate, pt.targetSite)

  /** Lens to access the rule witnessing the PhosphoTarget.
    * Make sure that you only update the rule with a "compatible" rule, e.g. refined rule, so that
    * the kinase and substrate indices don't become invalid.
    */
  def witness[Var[_]]: Lens[PhosphoTarget[Var], Rule[Var]] =
    Lens(pt => Store(r => PhosphoTarget(r, pt.kinaseIndex, pt.substrateIndex, pt.targetSite), pt.witness))


  type Ref[Var[_]] = Var[Discrete[PhosphoTarget[Var]]]

  implicit def showInstance[Var[_]]: Show[PhosphoTarget[Var]] = new Show[PhosphoTarget[Var]] {
    override def shows(p: PhosphoTarget[Var]): String = p.toString
  }

  implicit val deepShowKInstance: DeepShowK[PhosphoTarget] = new DeepShowK[PhosphoTarget] {
    def show[Ptr[_], M[_]](a: PhosphoTarget[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      M(a.kinase) >> M.write(" phosphorylates ") >> M(a.substrate) >> M.write(" at site ") >> M(a.targetSite)
  }

  implicit val deepEqualKInstance: DeepEqualK[PhosphoTarget, PhosphoTarget] =
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
      import proteinrefinery.util.Identification.Syntax._

      val pt = PhosphoTarget(kinase, substrate, targetSite)
      pt.focus(witness[Var] andThen Rule.lhs[Var]).putsf(lhs => {
        AgentsPatternOps.requireAssoc(pt.kinaseIndex, pt.substrateIndex, a => !(ISite(a.bindings.last.rightS) necessarilySame targetSite)).exec(lhs)
      })
    }

    def define(pt: PhosphoTriple[Var])(implicit M: Monad[M], E: EqualK[Var]): M[PhosphoTarget[Var]] =
      define(pt.kinase, pt.substrate, pt.targetSite)
  }

}