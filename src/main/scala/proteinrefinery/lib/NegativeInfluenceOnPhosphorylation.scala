package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Antichain, IncSet, Propagation}
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContU

import scalaz.{Monad, Show}

sealed trait NegativeInfluenceOnPhosphorylation

object NegativeInfluenceOnPhosphorylation {

  type Ref = Antichain.Ref[NegativeInfluenceOnPhosphorylation]

  // Constructors

  case class ByNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation) extends NegativeInfluenceOnPhosphorylation
  def        byNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation):        NegativeInfluenceOnPhosphorylation = ByNegativeInfluenceOnAssociation(value)
  def byCompetitiveBinding(cb: CompetitiveBinding): NegativeInfluenceOnPhosphorylation = byNegativeInfluenceOnAssociation(NegativeInfluenceOnAssociation.byCompetitiveBinding(cb))


  trait Search[M[_]] {
    implicit def Propagation: Propagation[M]

    def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search[M]

    def negativeInfluenceOnPhosphorylation(p: Protein, ph: Phosphorylation)(implicit M: Monad[M]): M[IncSetRef[Ref]] = {
      IncSet.collect(negativeInfluenceOnPhosphorylationC(p, ph))
    }

    def negativeInfluenceOnPhosphorylation_r(p: Protein, phRef: Phosphorylation.Ref)(implicit M: Monad[M]): M[IncSetRef[Ref]] = {
      IncSet.collect(negativeInfluenceOnPhosphorylationC_r(p, phRef))
    }

    def negativeInfluenceOnPhosphorylationC(p: Protein, ph: Phosphorylation)(implicit M: Monad[M]): ContU[M, Ref] = {
      // currently the only way a protein can have negative influence on phosphorylation
      // is via negative influence on the association of the enzyme to the substrate
      Antichain.map(NegativeInfluenceOnAssociationSearch.negativeInfluenceOnAssociationC(p, ph.assoc))(byNegativeInfluenceOnAssociation)
    }

    def negativeInfluenceOnPhosphorylationC_r(p: Protein, phRef: Phosphorylation.Ref)(implicit M: Monad[M]): ContU[M, Ref] = {
      // currently the only way a protein can have negative influence on phosphorylation
      // is via negative influence on the association of the enzyme to the substrate
      phRef.asCont[M] flatMap { negativeInfluenceOnPhosphorylationC(p, _) }
    }

  }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnPhosphorylation] = new Show[NegativeInfluenceOnPhosphorylation] {
    override def shows(x: NegativeInfluenceOnPhosphorylation): String = x match {
      case ByNegativeInfluenceOnAssociation(ni) => Show[NegativeInfluenceOnAssociation].shows(ni)
    }
  }
}