package proteinrefinery.lib

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.ContF
import KB._
import proteinrefinery._

import scalaz.Show

object AssocSearch {

  def search(p: Protein, q: Protein): Prg[IncSetRef[Assoc]] =
    IncSet.collect(searchC(p, q))

  def searchC(p: Protein, q: Protein): ContF[DSL, Assoc] =
    search0(Nil, p, q, Nil)

  private def search0(leftTail: List[Binding], p: Protein, q: Protein, rightTail: List[Binding]): ContF[DSL, Assoc] =
    bindingsOfC[DSL](p) flatMap { b =>
      if(leftTail.nonEmpty && b.leftS == leftTail.head.rightS) ContF.noop
      else if(leftTail.contains(b) || rightTail.contains(b)) ContF.noop
      else {
        val indirect0 = search0(b :: leftTail, b.right, q, rightTail)
        val indirect = DeferLang.deferC(Cost.complexity(10), indirect0)
        if(b.right == q) {
          val direct = ContF.point[DSL, Assoc](Assoc(leftTail reverse_::: b :: rightTail))
          ContF.sequence(direct, indirect)
        } else
          indirect
      }
    }


  def negativeInfluence(p: Protein, a: Assoc): Prg[IncSetRef[NegativeInfluenceOnAssociation]] =
    IncSet.collectAll(a.bindings.map(b => negativeInfluenceByCompetitiveBinding(p, b)))

  def negativeInfluenceC(p: Protein, a: Assoc): ContF[DSL, NegativeInfluenceOnAssociation] =
    ContF.sequence(a.bindings.map(b => negativeInfluenceByCompetitiveBinding(p, b)))

  private def negativeInfluenceByCompetitiveBinding(competitor: Protein, bnd: Binding): ContF[DSL, NegativeInfluenceOnAssociation] = {
    val l = competitiveBindings(competitor, bnd.leftPattern) map { competingBinding => NegativeInfluenceOnAssociation.byCompetitiveBinding(CompetitiveBinding(bnd.flip, competingBinding)) }
    val r = competitiveBindings(competitor, bnd.rightPattern) map { competingBinding => NegativeInfluenceOnAssociation.byCompetitiveBinding(CompetitiveBinding(bnd, competingBinding)) }
    ContF.sequence(l, r)
  }

  private def competitiveBindings(competitor: Protein, bp: BindingPartnerPattern): ContF[DSL, Binding] =
    ContF.filter(bindingsOfC[DSL](competitor)) { bnd =>
      bnd.right == bp.p.protein &&
      bnd.rightS == bp.s &&
      (bnd.rightPattern.p.mods combine bp.p.mods).isDefined
    }
}

sealed trait NegativeInfluenceOnAssociation

object NegativeInfluenceOnAssociation {

  final case class ByCompetitiveBinding(value: CompetitiveBinding) extends NegativeInfluenceOnAssociation
  def byCompetitiveBinding(cb: CompetitiveBinding): NegativeInfluenceOnAssociation = ByCompetitiveBinding(cb)

  implicit def showInstance: Show[NegativeInfluenceOnAssociation] = new Show[NegativeInfluenceOnAssociation] {
    override def shows(x: NegativeInfluenceOnAssociation): String = x match {
      case ByCompetitiveBinding(cb) => Show[CompetitiveBinding].shows(cb)
    }
  }
}