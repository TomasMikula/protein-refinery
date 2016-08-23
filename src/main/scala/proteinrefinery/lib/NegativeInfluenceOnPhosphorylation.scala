package proteinrefinery.lib

import nutcracker.IncSet
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.util.Antichain
import proteinrefinery.{DSL, Prg}

import scalaz.Show

sealed trait NegativeInfluenceOnPhosphorylation

object NegativeInfluenceOnPhosphorylation {

  type Ref = Antichain.Ref[NegativeInfluenceOnPhosphorylation]

  // Constructors

  case class ByNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation) extends NegativeInfluenceOnPhosphorylation
  def        byNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation):        NegativeInfluenceOnPhosphorylation = ByNegativeInfluenceOnAssociation(value)
  def byCompetitiveBinding(cb: CompetitiveBinding): NegativeInfluenceOnPhosphorylation = byNegativeInfluenceOnAssociation(NegativeInfluenceOnAssociation.byCompetitiveBinding(cb))


  // Search

  def search(p: Protein, ph: Phosphorylation): Prg[IncSetRef[Ref]] = {
    IncSet.collect(searchC(p, ph))
  }

  def search_r(p: Protein, phRef: Phosphorylation.Ref): Prg[IncSetRef[Ref]] = {
    IncSet.collect(searchC_r(p, phRef))
  }

  def searchC(p: Protein, ph: Phosphorylation): ContF[DSL, Ref] = {
    // currently the only way a protein can have negative influence on phosphorylation
    // is via negative influence on the association of the enzyme to the substrate
    Antichain.map(NegativeInfluenceOnAssociation.searchC(p, ph.assoc))(byNegativeInfluenceOnAssociation)
  }

  def searchC_r(p: Protein, phRef: Phosphorylation.Ref): ContF[DSL, Ref] = {
    // currently the only way a protein can have negative influence on phosphorylation
    // is via negative influence on the association of the enzyme to the substrate
    phRef.asCont[DSL] flatMap { searchC(p, _) }
  }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnPhosphorylation] = new Show[NegativeInfluenceOnPhosphorylation] {
    override def shows(x: NegativeInfluenceOnPhosphorylation): String = x match {
      case ByNegativeInfluenceOnAssociation(ni) => Show[NegativeInfluenceOnAssociation].shows(ni)
    }
  }
}