package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker._
import nutcracker.util.{ContU, EqualK}

import scalaz.{Monad, Show}

sealed trait NegativeInfluenceOnPhosphorylation[Ref[_]]

object NegativeInfluenceOnPhosphorylation {

  type Ref[Var[_]] = Var[Antichain[NegativeInfluenceOnPhosphorylation[Var]]]

  // Constructors

  case class ByNegativeInfluenceOnAssociation[Var[_]](value: NegativeInfluenceOnAssociation[Var]) extends NegativeInfluenceOnPhosphorylation[Var]
  def        byNegativeInfluenceOnAssociation[Var[_]](value: NegativeInfluenceOnAssociation[Var]):        NegativeInfluenceOnPhosphorylation[Var] = ByNegativeInfluenceOnAssociation(value)
  def byCompetitiveBinding[Var[_]](cb: CompetitiveBinding[Var]): NegativeInfluenceOnPhosphorylation[Var] = byNegativeInfluenceOnAssociation(NegativeInfluenceOnAssociation.byCompetitiveBinding(cb))


  trait Search[M[_], Var[_]] {
    implicit def Propagation: Propagation[M, Var]

    def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search[M, Var]
    def IncSets: nutcracker.IncSets[M, Var]

    def negativeInfluenceOnPhosphorylation(p: Protein, ph: Phosphorylation[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[Ref[Var]]]] = {
      IncSets.collect(negativeInfluenceOnPhosphorylationC(p, ph))
    }

    def negativeInfluenceOnPhosphorylation_r(p: Protein, phRef: Phosphorylation.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[Ref[Var]]]] = {
      IncSets.collect(negativeInfluenceOnPhosphorylationC_r(p, phRef))
    }

    def negativeInfluenceOnPhosphorylationC(p: Protein, ph: Phosphorylation[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
      // currently the only way a protein can have negative influence on phosphorylation
      // is via negative influence on the association of the enzyme to the substrate
      Antichain.map(NegativeInfluenceOnAssociationSearch.negativeInfluenceOnAssociationC(p, ph.assoc))(byNegativeInfluenceOnAssociation)
    }

    def negativeInfluenceOnPhosphorylationC_r(p: Protein, phRef: Phosphorylation.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
      // currently the only way a protein can have negative influence on phosphorylation
      // is via negative influence on the association of the enzyme to the substrate
      phRef.asCont[M] flatMap { negativeInfluenceOnPhosphorylationC(p, _) }
    }

  }


  // Typeclass instances

  implicit def showInstance[Var[_]]: Show[NegativeInfluenceOnPhosphorylation[Var]] = new Show[NegativeInfluenceOnPhosphorylation[Var]] {
    override def shows(x: NegativeInfluenceOnPhosphorylation[Var]): String = x match {
      case ByNegativeInfluenceOnAssociation(ni) => Show[NegativeInfluenceOnAssociation[Var]].shows(ni)
    }
  }
}