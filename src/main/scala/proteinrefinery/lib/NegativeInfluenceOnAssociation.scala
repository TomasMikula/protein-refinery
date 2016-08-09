package proteinrefinery.lib

import nutcracker.IncSet
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContF
import proteinrefinery.{DSL, Prg}

import scalaz.Show

sealed trait NegativeInfluenceOnAssociation

object NegativeInfluenceOnAssociation {

  // Constructors

  final case class ByCompetitiveBinding(value: CompetitiveBinding) extends NegativeInfluenceOnAssociation
  def byCompetitiveBinding(cb: CompetitiveBinding): NegativeInfluenceOnAssociation = ByCompetitiveBinding(cb)


  // Search

  def search(p: Protein, a: Assoc): Prg[IncSetRef[NegativeInfluenceOnAssociation]] =
    IncSet.collectAll(a.bindings.map(b => negativeInfluenceByCompetitiveBinding(p, b)))

  def searchC(p: Protein, a: Assoc): ContF[DSL, NegativeInfluenceOnAssociation] =
    ContF.sequence(a.bindings.map(b => negativeInfluenceByCompetitiveBinding(p, b)))

  private def negativeInfluenceByCompetitiveBinding(competitor: Protein, bnd: Binding): ContF[DSL, NegativeInfluenceOnAssociation] = {
    val l = competitiveBindings(competitor, bnd.leftPattern) map { competingBinding => NegativeInfluenceOnAssociation.byCompetitiveBinding(CompetitiveBinding(bnd.flip, competingBinding)) }
    val r = competitiveBindings(competitor, bnd.rightPattern) map { competingBinding => NegativeInfluenceOnAssociation.byCompetitiveBinding(CompetitiveBinding(bnd, competingBinding)) }
    ContF.sequence(l, r)
  }

  private def competitiveBindings(competitor: Protein, bp: BindingPartnerPattern): ContF[DSL, Binding] =
    ContF.filter(KB.bindingsOfC[DSL](competitor)) { bnd =>
      bnd.right == bp.p.protein &&
        bnd.rightS == bp.s &&
        (bnd.rightPattern.p.mods combine bp.p.mods).isDefined
    }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnAssociation] = new Show[NegativeInfluenceOnAssociation] {
    override def shows(x: NegativeInfluenceOnAssociation): String = x match {
      case ByCompetitiveBinding(cb) => Show[CompetitiveBinding].shows(cb)
    }
  }
}