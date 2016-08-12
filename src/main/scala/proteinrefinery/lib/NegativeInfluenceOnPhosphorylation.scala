package proteinrefinery.lib

import nutcracker.IncSet
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.util.Antichain
import proteinrefinery.{DSL, DSL2, Prg}

import scalaz.Show

sealed trait NegativeInfluenceOnPhosphorylation

object NegativeInfluenceOnPhosphorylation {

  type Ref = Antichain.Ref[NegativeInfluenceOnPhosphorylation]

  // Constructors

  case class ByNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation) extends NegativeInfluenceOnPhosphorylation
  def        byNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation):        NegativeInfluenceOnPhosphorylation = ByNegativeInfluenceOnAssociation(value)
  def byCompetitiveBinding(cb: CompetitiveBinding): NegativeInfluenceOnPhosphorylation = byNegativeInfluenceOnAssociation(NegativeInfluenceOnAssociation.byCompetitiveBinding(cb))


  // Search

  def search(p: Protein, ph: Phosphorylation): Prg[IncSetRef[NegativeInfluenceOnPhosphorylation]] = {
    IncSet.collect(searchC(p, ph))
  }

  def searchC(p: Protein, ph: Phosphorylation): ContF[DSL, NegativeInfluenceOnPhosphorylation] = {
    // currently the only way a protein can have negative influence on phosphorylation
    // is via negative influence on the association of enzyme and substrate
    NegativeInfluenceOnAssociation.searchC(p, ph.assoc).map(byNegativeInfluenceOnAssociation)
  }

  def searchC_2(p: Protein, ph: Phosphorylation): ContF[DSL2, Ref] = {
    // currently the only way a protein can have negative influence on phosphorylation
    // is via negative influence on the association of the enzyme to the substrate
    Antichain.map(NegativeInfluenceOnAssociation.searchC_2(p, ph.assoc))(byNegativeInfluenceOnAssociation)
  }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnPhosphorylation] = new Show[NegativeInfluenceOnPhosphorylation] {
    override def shows(x: NegativeInfluenceOnPhosphorylation): String = x match {
      case ByNegativeInfluenceOnAssociation(ni) => Show[NegativeInfluenceOnAssociation].shows(ni)
    }
  }
}