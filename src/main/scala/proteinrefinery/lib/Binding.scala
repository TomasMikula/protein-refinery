package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Antichain
import nutcracker.util.{DeepEqualK, EqualK, IsEqual}
import proteinrefinery.lib.ProteinModifications.LocalSiteId

sealed trait Binding[Ref[_]] {

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
  def p: Protein = witness.lhs(pi).protein

  /** second binding partner */
  def q: Protein = witness.lhs(qi).protein

  /** Alias for [[p]]. */
  def left = p

  /** Alias for [[ps]]. */
  def leftS = ps

  /** Alias for [[q]]. */
  def right = q

  /** Alias for [[qs]]. */
  def rightS = qs

  def pPattern: BindingPartnerPattern[Ref] = BindingPartnerPattern(witness.lhs(pi), ps)
  def qPattern: BindingPartnerPattern[Ref] = BindingPartnerPattern(witness.lhs(qi), qs)
  def leftPattern = pPattern
  def rightPattern = qPattern

  def flip: Binding[Ref] = Binding(witness, qi, pi, qs, ps)

  override def toString = s"$p>$ps - $qs<$q"
}

object Binding {
  private case class Binding0[Var[_]](witness: Rule[Var], pi: AgentIndex, qi: AgentIndex, ps: LocalSiteId[Var], qs: LocalSiteId[Var]) extends Binding[Var]

  type Ref[Var[_]] = Var[Antichain[Binding[Var]]]

  def apply[Var[_]](witness: Rule[Var], pi: AgentIndex, qi: AgentIndex, ps: LocalSiteId[Var], qs: LocalSiteId[Var]): Binding[Var] =
    Binding0(witness, pi, qi, ps, qs)

  def apply[Var[_]](p: Protein, ps: LocalSiteId[Var], q: Protein, qs: LocalSiteId[Var])(implicit ev: EqualK[Var]): Binding[Var] =
    BindingPartnerPattern(p, ps) bind BindingPartnerPattern(q, qs)

  implicit val deepEqualKInstance: DeepEqualK[Binding, Binding] = new DeepEqualK[Binding, Binding] {
    def equal[Ptr1[_], Ptr2[_]](b1: Binding[Ptr1], b2: Binding[Ptr2]): IsEqual[Ptr1, Ptr2] =
      IsEqual(b1.witness, b2.witness) && IsEqual(b1.pi, b2.pi) && IsEqual(b1.qi, b2.qi) && IsEqual(b1.ps, b2.ps) && IsEqual(b1.qs, b2.qs)
  }
}