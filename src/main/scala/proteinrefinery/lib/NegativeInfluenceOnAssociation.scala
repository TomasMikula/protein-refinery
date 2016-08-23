package proteinrefinery.lib

import nutcracker.{Antichain, IncSet}
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.{DSL, Prg}

import scalaz.Show

sealed trait NegativeInfluenceOnAssociation

object NegativeInfluenceOnAssociation {

  type Ref = Antichain.Ref[NegativeInfluenceOnAssociation]

  // Constructors

  case class ByCompetitiveBinding(value: CompetitiveBinding) extends NegativeInfluenceOnAssociation
  def        byCompetitiveBinding(value: CompetitiveBinding):        NegativeInfluenceOnAssociation = ByCompetitiveBinding(value)


  // Search

  // TODO: should return DSet
  def search(p: Protein, a: Assoc): Prg[IncSetRef[Ref]] =
    IncSet.collectAll(a.bindings.map(b => searchCompetitiveBinding(p, b)))

  def searchC(p: Protein, a: Assoc): ContF[DSL, Ref] =
    ContF.sequence(a.bindings.map(b => searchCompetitiveBinding(p, b)))

  private def searchCompetitiveBinding(competitor: Protein, bnd: Binding): ContF[DSL, Ref] = {
    val l = Antichain.map(competitiveBindings(competitor, bnd.leftPattern ))(cb => byCompetitiveBinding(CompetitiveBinding(bnd.flip, cb)))
    val r = Antichain.map(competitiveBindings(competitor, bnd.rightPattern))(cb => byCompetitiveBinding(CompetitiveBinding(bnd,      cb)))
    ContF.sequence(l, r)
  }

  // TODO: return Prg[DSet[Binding]] instead
  private def competitiveBindings(competitor: Protein, bp: BindingPartnerPattern): ContF[DSL, Binding.Ref] =
    ContF.filterMap(Nuggets.bindingsOfC[DSL](competitor).flatMap(ref => ref.asCont[DSL].map((ref, _)))) {
      case (ref, bnd) => if(bnd.rightPattern overlaps bp) Option(ref) else None
    }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnAssociation] = new Show[NegativeInfluenceOnAssociation] {
    override def shows(x: NegativeInfluenceOnAssociation): String = x match {
      case ByCompetitiveBinding(cb) => Show[CompetitiveBinding].shows(cb)
    }
  }
}