package protein.lib

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.ContF
import KB._
import protein._

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


  def negativeInfluence(p: Protein, a: Assoc): Prg[IncSetRef[CompetitiveBinding]] =
    IncSet.collectAll(a.bindings.map(b => competitiveBindings0(p, b)))

  def negativeInfluenceC(p: Protein, a: Assoc): ContF[DSL, CompetitiveBinding] =
    ContF.sequence(a.bindings.map(b => competitiveBindings0(p, b)))

  private def competitiveBindings0(competitor: Protein, bnd: Binding): ContF[DSL, CompetitiveBinding] = {
    val l = competitiveBindings1(competitor, bnd.leftPattern) map { competingBinding => CompetitiveBinding(bnd.flip, competingBinding) }
    val r = competitiveBindings1(competitor, bnd.rightPattern) map { competingBinding => CompetitiveBinding(bnd, competingBinding) }
    ContF.sequence(l, r)
  }

  private def competitiveBindings1(competitor: Protein, bp: BindingPartnerPattern): ContF[DSL, Binding] =
    ContF.filter(bindingsOfC[DSL](competitor)) { bnd =>
      bnd.right == bp.p.protein &&
      bnd.rightS == bp.s &&
      (bnd.rightPattern.p.mods combine bp.p.mods).isDefined
    }
}