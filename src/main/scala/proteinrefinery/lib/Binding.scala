package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Discrete, RelPSource}
import nutcracker.util.{DeepEqualK, EqualK, IsEqual}
import proteinrefinery.lib.ProteinModifications.LocalSiteId

import scalaz.Equal
import scalaz.syntax.equal._

sealed trait Binding[Ref[_]] {

  /** Rule that witnesses this binding action to occur. */
  def witness: Rule.Ref[Ref]

  /** Index of [[p]] in both LHS and RHS of [[witness]]. */
  def pi: AgentIndex

  /** Index of [[q]] in both LHS and RHS of [[witness]]. */
  def qi: AgentIndex

  /** binding site at [[p]] */
  def ps: LocalSiteId[Ref]

  /** binding site at [[q]] */
  def qs: LocalSiteId[Ref]

  /** first binding partner */
  def p: RelPSource[Ref, Discrete[Protein]] = protein(pi)

  /** second binding partner */
  def q: RelPSource[Ref, Discrete[Protein]] = protein(qi)

  /** Alias for [[p]]. */
  def left = p

  def getLeft(r: Rule[Ref]): Protein = r.lhs(pi).protein

  /** Alias for [[ps]]. */
  def leftS = ps

  /** Alias for [[q]]. */
  def right = q

  def getRight(r: Rule[Ref]): Protein = r.lhs(qi).protein

  /** Alias for [[qs]]. */
  def rightS = qs

  def pPattern: RelPSource[Ref, Discrete[BindingPartnerPattern[Ref]]] = partner(pi, ps) // BindingPartnerPattern(witness.lhs(pi), ps)
  def qPattern: RelPSource[Ref, Discrete[BindingPartnerPattern[Ref]]] = partner(qi, qs) // BindingPartnerPattern(witness.lhs(qi), qs)
  def leftPattern = pPattern
  def rightPattern = qPattern

  def getLeftPattern(r: Rule[Ref]): BindingPartnerPattern[Ref] = BindingPartnerPattern(r.lhs(pi), ps)
  def getRightPattern(r: Rule[Ref]): BindingPartnerPattern[Ref] = BindingPartnerPattern(r.lhs(qi), qs)

  def flip: Binding[Ref] = Binding(witness, qi, pi, qs, ps)

  override def toString = s"$p>$ps - $qs<$q"

  private def protein(i: AgentIndex): RelPSource[Ref, Discrete[Protein]] =
    RelPSource.lift(witness).map(r => Discrete(r.value.lhs(i).protein)).deltas(x => x)

  private def partner(i: AgentIndex, si: LocalSiteId[Ref]): RelPSource[Ref, Discrete[BindingPartnerPattern[Ref]]] =
    RelPSource.lift(witness).map(r => Discrete(BindingPartnerPattern(r.value.lhs(i), si))).deltas(x => x)
}

object Binding {
  private case class Binding0[Var[_]](witness: Rule.Ref[Var], pi: AgentIndex, qi: AgentIndex, ps: LocalSiteId[Var], qs: LocalSiteId[Var]) extends Binding[Var]

  type Ref[Var[_]] = Var[Discrete[Binding[Var]]]

  def apply[Var[_]](witness: Rule.Ref[Var], pi: AgentIndex, qi: AgentIndex, ps: LocalSiteId[Var], qs: LocalSiteId[Var]): Binding[Var] =
    Binding0(witness, pi, qi, ps, qs)

  def apply[Var[_]](witness: Rule.Ref[Var], link: Link[Var]): Binding[Var] = link match {
    case Link(i, si, j, sj) => Binding0(witness, i, j, si, sj)
  }

  implicit val deepEqualKInstance: DeepEqualK[Binding, Binding] = new DeepEqualK[Binding, Binding] {
    def equal[Ptr1[_], Ptr2[_]](b1: Binding[Ptr1], b2: Binding[Ptr2]): IsEqual[Ptr1, Ptr2] =
      IsEqual(b1.witness, b2.witness) && IsEqual(b1.pi, b2.pi) && IsEqual(b1.qi, b2.qi) && IsEqual(b1.ps, b2.ps) && IsEqual(b1.qs, b2.qs)
  }

  implicit def equalInstance[Var[_]](implicit ev: EqualK[Var]): Equal[Binding[Var]] = new Equal[Binding[Var]] {
    import EqualK._
    def equal(b1: Binding[Var], b2: Binding[Var]): Boolean =
      b1.witness === b2.witness &&
      b1.pi === b2.pi &&
      b1.qi === b2.qi &&
      b1.ps === b2.ps &&
      b1.qs === b2.qs
  }
}

sealed trait BindingData[Ref[_]] {

  /** Rule that witnesses this binding action to occur. */
  def witness: Rule[Ref]

  /** Index of [[p]] in both LHS and RHS of [[witness]]. */
  def pi: AgentIndex

  /** Index of [[q]] in both LHS and RHS of [[witness]]. */
  def qi: AgentIndex

  /** binding site at [[p]] */
  def ps: LocalSiteId[Ref]

  /** binding site at [[q]] */
  def qs: LocalSiteId[Ref]

  /** first binding partner */
  def p: Protein = protein(pi)

  /** second binding partner */
  def q: Protein = protein(qi)

  /** Alias for [[p]]. */
  def left = p

  /** Alias for [[ps]]. */
  def leftS = ps

  /** Alias for [[q]]. */
  def right = q

  /** Alias for [[qs]]. */
  def rightS = qs

  def pPattern: BindingPartnerPattern[Ref] = partner(pi, ps)
  def qPattern: BindingPartnerPattern[Ref] = partner(qi, qs)
  def leftPattern = pPattern
  def rightPattern = qPattern

  def flip: BindingData[Ref] = BindingData(witness, qi, pi, qs, ps)

  def link: Link[Ref] = Link(pi, ps, qi, qs)

  override def toString = s"$p>$ps - $qs<$q"

  private def protein(i: AgentIndex): Protein =
    witness.lhs(i).protein

  private def partner(i: AgentIndex, si: LocalSiteId[Ref]): BindingPartnerPattern[Ref] =
    BindingPartnerPattern(witness.lhs(i), si)
}

object BindingData {
  private case class BindingData0[Var[_]](witness: Rule[Var], pi: AgentIndex, qi: AgentIndex, ps: LocalSiteId[Var], qs: LocalSiteId[Var]) extends BindingData[Var]

  def apply[Var[_]](witness: Rule[Var], pi: AgentIndex, qi: AgentIndex, ps: LocalSiteId[Var], qs: LocalSiteId[Var]): BindingData[Var] =
    BindingData0(witness, pi, qi, ps, qs)

  def apply[Var[_]](p: Protein, ps: LocalSiteId[Var], q: Protein, qs: LocalSiteId[Var])(implicit ev: EqualK[Var]): BindingData[Var] =
    BindingPartnerPattern(p, ps) bind BindingPartnerPattern(q, qs)

  implicit val deepEqualKInstance: DeepEqualK[Binding, Binding] = new DeepEqualK[Binding, Binding] {
    def equal[Ptr1[_], Ptr2[_]](b1: Binding[Ptr1], b2: Binding[Ptr2]): IsEqual[Ptr1, Ptr2] =
      IsEqual(b1.witness, b2.witness) && IsEqual(b1.pi, b2.pi) && IsEqual(b1.qi, b2.qi) && IsEqual(b1.ps, b2.ps) && IsEqual(b1.qs, b2.qs)
  }
}