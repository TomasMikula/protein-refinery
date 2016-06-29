package protein.search

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.{ContF, FreeK}
import protein._
import protein.KBLang._
import protein.capability.BindingPartnerPattern
import protein.mechanism.{Assoc, Binding, CompetitiveBinding, Protein}

import scalaz.syntax.monad._

object AssocSearch {

  def search(p: Protein, q: Protein): Prg[IncSetRef[Assoc]] =
    IncSet.collect(searchC(p, q))

  def searchC(p: Protein, q: Protein): Cont[Assoc] =
    search0(Nil, p, q, Nil)

  private def search0(leftTail: List[Binding], p: Protein, q: Protein, rightTail: List[Binding]): Cont[Assoc] =
    bindingsOfC[DSL](p) flatMap { b =>
      if(leftTail.nonEmpty && b.leftS == leftTail.head.rightS) Cont.noop
      else if(leftTail.contains(b) || rightTail.contains(b)) Cont.noop
      else {
        val indirect0 = search0(b :: leftTail, b.right, q, rightTail)
        val indirect = DeferLang.deferC(Cost.complexity(10), indirect0)
        if(b.right == q) {
          val direct = Assoc(leftTail reverse_::: b :: rightTail).point[Cont]
          Cont.sequence(direct, indirect)
        } else
          indirect
      }
    }


  def negativeInfluence(p: Protein, a: Assoc): Prg[IncSetRef[CompetitiveBinding]] =
    IncSet.collectAll(a.bindings.map(b => competitiveBindings0(p, b)))

  def negativeInfluenceC(p: Protein, a: Assoc): Cont[CompetitiveBinding] =
    ContF.sequence(a.bindings.map(b => competitiveBindings0(p, b)))

  private def competitiveBindings0(competitor: Protein, bnd: Binding): Cont[CompetitiveBinding] = {
    val l = competitiveBindings1(competitor, bnd.leftPattern) map { competingBinding => CompetitiveBinding(bnd.flip, competingBinding) }
    val r = competitiveBindings1(competitor, bnd.rightPattern) map { competingBinding => CompetitiveBinding(bnd, competingBinding) }
    ContF.sequence(l, r)
  }

  private def competitiveBindings1(competitor: Protein, bp: BindingPartnerPattern): Cont[Binding] =
    ContF.filter(bindingsOfC[DSL](competitor)) { bnd =>
      bnd.right == bp.p.protein &&
      bnd.rightS == bp.s &&
      (bnd.rightPattern.p.mods combine bp.p.mods).isDefined
    }
}