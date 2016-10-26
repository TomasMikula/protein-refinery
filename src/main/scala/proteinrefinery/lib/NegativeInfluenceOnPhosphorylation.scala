package proteinrefinery.lib

import nutcracker.{Antichain, IncSet}
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.{DSL, Prg}

import scalaz.Show

sealed trait NegativeInfluenceOnPhosphorylation

object NegativeInfluenceOnPhosphorylation {

  type Ref = Antichain.Ref[NegativeInfluenceOnPhosphorylation]

  // Constructors

  case class ByNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation) extends NegativeInfluenceOnPhosphorylation
  def        byNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation):        NegativeInfluenceOnPhosphorylation = ByNegativeInfluenceOnAssociation(value)
  def byCompetitiveBinding(cb: CompetitiveBinding): NegativeInfluenceOnPhosphorylation = byNegativeInfluenceOnAssociation(NegativeInfluenceOnAssociation.byCompetitiveBinding(cb))


  trait Search {

    def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search

    def negativeInfluenceOnPhosphorylation(p: Protein, ph: Phosphorylation): Prg[IncSetRef[Ref]] = {
      IncSet.collect(negativeInfluenceOnPhosphorylationC(p, ph))
    }

    def negativeInfluenceOnPhosphorylation_r(p: Protein, phRef: Phosphorylation.Ref): Prg[IncSetRef[Ref]] = {
      IncSet.collect(negativeInfluenceOnPhosphorylationC_r(p, phRef))
    }

    def negativeInfluenceOnPhosphorylationC(p: Protein, ph: Phosphorylation): ContF[DSL, Ref] = {
      // currently the only way a protein can have negative influence on phosphorylation
      // is via negative influence on the association of the enzyme to the substrate
      Antichain.map(NegativeInfluenceOnAssociationSearch.negativeInfluenceOnAssociationC(p, ph.assoc))(byNegativeInfluenceOnAssociation)
    }

    def negativeInfluenceOnPhosphorylationC_r(p: Protein, phRef: Phosphorylation.Ref): ContF[DSL, Ref] = {
      // currently the only way a protein can have negative influence on phosphorylation
      // is via negative influence on the association of the enzyme to the substrate
      phRef.asCont[DSL] flatMap { negativeInfluenceOnPhosphorylationC(p, _) }
    }

  }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnPhosphorylation] = new Show[NegativeInfluenceOnPhosphorylation] {
    override def shows(x: NegativeInfluenceOnPhosphorylation): String = x match {
      case ByNegativeInfluenceOnAssociation(ni) => Show[NegativeInfluenceOnAssociation].shows(ni)
    }
  }
}