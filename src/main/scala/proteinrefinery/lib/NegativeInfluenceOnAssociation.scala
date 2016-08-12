package proteinrefinery.lib

import nutcracker.IncSet
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.util.Antichain
import proteinrefinery.{DSL, DSL2, Prg, Prg2}

import scalaz.Show

sealed trait NegativeInfluenceOnAssociation

object NegativeInfluenceOnAssociation {

  type Ref = Antichain.Ref[NegativeInfluenceOnAssociation]

  // Constructors

  case class ByCompetitiveBinding(value: CompetitiveBinding) extends NegativeInfluenceOnAssociation
  def        byCompetitiveBinding(value: CompetitiveBinding):        NegativeInfluenceOnAssociation = ByCompetitiveBinding(value)


  // Search

  def search(p: Protein, a: Assoc): Prg[IncSetRef[NegativeInfluenceOnAssociation]] =
    IncSet.collectAll(a.bindings.map(b => searchCompetitiveBinding(p, b)))

  // TODO: should return DSet
  def search_2(p: Protein, a: Assoc): Prg2[IncSetRef[Ref]] =
    IncSet.collectAll(a.bindings.map(b => searchCompetitiveBinding_2(p, b)))

  def searchC(p: Protein, a: Assoc): ContF[DSL, NegativeInfluenceOnAssociation] =
    ContF.sequence(a.bindings.map(b => searchCompetitiveBinding(p, b)))

  def searchC_2(p: Protein, a: Assoc): ContF[DSL2, Ref] =
    ContF.sequence(a.bindings.map(b => searchCompetitiveBinding_2(p, b)))

  private def searchCompetitiveBinding(competitor: Protein, bnd: Binding): ContF[DSL, NegativeInfluenceOnAssociation] = {
    val l = competitiveBindings(competitor, bnd.leftPattern ) map { competingBinding => byCompetitiveBinding(CompetitiveBinding(bnd.flip, competingBinding)) }
    val r = competitiveBindings(competitor, bnd.rightPattern) map { competingBinding => byCompetitiveBinding(CompetitiveBinding(bnd,      competingBinding)) }
    ContF.sequence(l, r)
  }

  private def searchCompetitiveBinding_2(competitor: Protein, bnd: Binding): ContF[DSL2, Ref] = {
    val l = Antichain.map(competitiveBindings_2(competitor, bnd.leftPattern ))(cb => byCompetitiveBinding(CompetitiveBinding(bnd.flip, cb)))
    val r = Antichain.map(competitiveBindings_2(competitor, bnd.rightPattern))(cb => byCompetitiveBinding(CompetitiveBinding(bnd,      cb)))
    ContF.sequence(l, r)
  }

  private def competitiveBindings(competitor: Protein, bp: BindingPartnerPattern): ContF[DSL, Binding] =
    ContF.filter(KB.bindingsOfC[DSL](competitor)) { _.rightPattern overlaps bp }

  // TODO: return Prg2[DSet[Binding]] instead
  private def competitiveBindings_2(competitor: Protein, bp: BindingPartnerPattern): ContF[DSL2, Binding.Ref] =
    ContF.filterMap(Nuggets.bindingsOfC[DSL2](competitor).flatMap(ref => ref.asCont[DSL2].map((ref, _)))) {
      case (ref, bnd) => if(bnd.rightPattern overlaps bp) Option(ref) else None
    }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnAssociation] = new Show[NegativeInfluenceOnAssociation] {
    override def shows(x: NegativeInfluenceOnAssociation): String = x match {
      case ByCompetitiveBinding(cb) => Show[CompetitiveBinding].shows(cb)
    }
  }
}