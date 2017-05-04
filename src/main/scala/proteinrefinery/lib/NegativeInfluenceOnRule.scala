package proteinrefinery.lib

import nutcracker.Propagation
import nutcracker.data.{Discrete, IncSet}
import nutcracker.ops._
import nutcracker.util.{ContU, DeepEqualK, EqualK, IsEqual}

import scalaz.{Monad, Show}

sealed trait NegativeInfluenceOnRule[Ref[_]]

object NegativeInfluenceOnRule {
  type Ref[Var[_]] = Var[Discrete[NegativeInfluenceOnRule[Var]]]

  case class ByNegativeInfluenceOnAssociation[Var[_]](value: NegativeInfluenceOnAssociation[Var]) extends NegativeInfluenceOnRule[Var]
  def        byNegativeInfluenceOnAssociation[Var[_]](value: NegativeInfluenceOnAssociation[Var]):        NegativeInfluenceOnRule[Var] = ByNegativeInfluenceOnAssociation(value)

  trait Search[M[_], Var[_], Val[_]] {
    protected implicit def Propagation: Propagation[M, Var, Val]

    def IncSets: nutcracker.data.IncSets[M, Var, Val]
    def AgentsPatternOps: AgentsPattern.Ops[M, Var, Val]
    def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search[M, Var, Val]

    def negativeInfluenceOnRule(p: Protein, ref: Rule.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[Ref[Var]]]] =
      IncSets.collect(negativeInfluenceOnRuleC_r(p, ref))

    def negativeInfluenceOnRuleC_r(p: Protein, ref: Rule.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] =
      ref.asCont_ flatMap (negativeInfluenceOnRuleC(p, _))

    def negativeInfluenceOnRuleC(p: Protein, r: Rule[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] =
      // currently the only way a protein can have negative influence on rule
      // is via negative influence on one of the associations
      for {
        aRef <- AgentsPatternOps.forEachAssoc(r.lhs)
        na <- NegativeInfluenceOnAssociationSearch.negativeInfluenceOnAssociationC(p, aRef)
        nr = byNegativeInfluenceOnAssociation(na)
        nrRef <- Discrete.cellC(nr)
      } yield nrRef
  }

  implicit val deepEqualKInstance: DeepEqualK[NegativeInfluenceOnRule, NegativeInfluenceOnRule] =
    new DeepEqualK[NegativeInfluenceOnRule, NegativeInfluenceOnRule] {
      def equal[Ptr1[_], Ptr2[_]](a1: NegativeInfluenceOnRule[Ptr1], a2: NegativeInfluenceOnRule[Ptr2]): IsEqual[Ptr1, Ptr2] = {
        val (ByNegativeInfluenceOnAssociation(na1), ByNegativeInfluenceOnAssociation(na2)) = (a1, a2)
        IsEqual(na1, na2)
      }
  }

  implicit def showInstance[Var[_]]: Show[NegativeInfluenceOnRule[Var]] = new Show[NegativeInfluenceOnRule[Var]] {
    override def shows(x: NegativeInfluenceOnRule[Var]): String = x match {
      case ByNegativeInfluenceOnAssociation(na) => Show[NegativeInfluenceOnAssociation[Var]].shows(na)
    }
  }
}