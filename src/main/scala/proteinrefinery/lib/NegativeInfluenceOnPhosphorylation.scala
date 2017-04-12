package proteinrefinery.lib

import scala.language.higherKinds

import nutcracker.{Discrete, IncSet, Propagation}
import nutcracker.ops._
import nutcracker.util.{ContU, DeepEqualK, EqualK, IsEqual}

import scalaz.{Monad, Show}

sealed trait NegativeInfluenceOnPhosphorylation[Ref[_]]

object NegativeInfluenceOnPhosphorylation {

  type Ref[Var[_]] = Var[Discrete[NegativeInfluenceOnPhosphorylation[Var]]]

  // Constructors

  case class ByNegativeInfluenceOnRule[Var[_]](value: NegativeInfluenceOnRule[Var]) extends NegativeInfluenceOnPhosphorylation[Var]
  def        byNegativeInfluenceOnRule[Var[_]](value: NegativeInfluenceOnRule[Var]):        NegativeInfluenceOnPhosphorylation[Var] = ByNegativeInfluenceOnRule(value)

  def byCompetitiveBinding[Var[_]](cb: CompetitiveBinding[Var]): NegativeInfluenceOnPhosphorylation[Var] =
    byNegativeInfluenceOnRule(NegativeInfluenceOnRule.byNegativeInfluenceOnAssociation(NegativeInfluenceOnAssociation.byCompetitiveBinding(cb)))


  trait Search[M[_], Var[_], Val[_]] {
    protected implicit def Propagation: Propagation[M, Var, Val]

    def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search[M, Var, Val]
    def NegativeInfluenceOnRuleSearch: NegativeInfluenceOnRule.Search[M, Var, Val]
    def IncSets: nutcracker.IncSets[M, Var, Val]

    def negativeInfluenceOnPhosphorylation(p: Protein, ph: PhosphoTarget[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[Ref[Var]]]] = {
      IncSets.collect(negativeInfluenceOnPhosphorylationC(p, ph))
    }

    def negativeInfluenceOnPhosphorylation_r(p: Protein, phRef: PhosphoTarget.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[Ref[Var]]]] = {
      IncSets.collect(negativeInfluenceOnPhosphorylationC_r(p, phRef))
    }

    def negativeInfluenceOnPhosphorylationC(p: Protein, ph: PhosphoTarget[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] =
      Discrete.map(NegativeInfluenceOnRuleSearch.negativeInfluenceOnRuleC(p, ph.witness))(byNegativeInfluenceOnRule)

    def negativeInfluenceOnPhosphorylationC_r(p: Protein, ptRef: PhosphoTarget.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] =
      ptRef.asCont_ flatMap (negativeInfluenceOnPhosphorylationC(p, _))

  }


  // Typeclass instances

  implicit val deepEqualKInstance: DeepEqualK[NegativeInfluenceOnPhosphorylation, NegativeInfluenceOnPhosphorylation] =
    new DeepEqualK[NegativeInfluenceOnPhosphorylation, NegativeInfluenceOnPhosphorylation] {
      def equal[Ptr1[_], Ptr2[_]](a1: NegativeInfluenceOnPhosphorylation[Ptr1], a2: NegativeInfluenceOnPhosphorylation[Ptr2]): IsEqual[Ptr1, Ptr2] =
        (a1, a2) match {
          case (ByNegativeInfluenceOnRule(nr1), ByNegativeInfluenceOnRule(nr2)) => IsEqual(nr1, nr2)
          case _ => sys.error("Unreachable code")
        }
    }

  implicit def showInstance[Var[_]]: Show[NegativeInfluenceOnPhosphorylation[Var]] = new Show[NegativeInfluenceOnPhosphorylation[Var]] {
    override def shows(x: NegativeInfluenceOnPhosphorylation[Var]): String = x match {
      case ByNegativeInfluenceOnRule(nr) => Show[NegativeInfluenceOnRule[Var]].shows(nr)
    }
  }
}