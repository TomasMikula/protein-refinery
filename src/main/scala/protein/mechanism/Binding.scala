package protein.mechanism

import protein.capability.{AgentIndex, BindingPartnerPattern, Rule}

sealed trait Binding {

  /** Rule that witnesses this binding action to occur. */
  def witness: Rule

  /** Index of [[p]] in both LHS and RHS of [[witness]]. */
  def pi: AgentIndex

  /** Index of [[q]] in both LHS and RHS of [[witness]]. */
  def qi: AgentIndex

  /** binding site at [[p]] */
  def ps: Site

  /** binding site at [[q]] */
  def qs: Site

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

  def pPattern: BindingPartnerPattern = BindingPartnerPattern(witness.lhs(pi), ps)
  def qPattern: BindingPartnerPattern = BindingPartnerPattern(witness.lhs(qi), qs)
  def leftPattern = pPattern
  def rightPattern = qPattern

  def flip: Binding = Binding(witness, qi, pi, qs, ps)

  override def toString = s"$p>$ps - $qs<$q"
}

object Binding {
  private case class Binding0(witness: Rule, pi: AgentIndex, qi: AgentIndex, ps: Site, qs: Site) extends Binding

  def apply(witness: Rule, pi: AgentIndex, qi: AgentIndex, ps: Site, qs: Site): Binding =
    Binding0(witness, pi, qi, ps, qs)

  def apply(p: Protein, ps: Site, q: Protein, qs: Site): Binding =
    BindingPartnerPattern(p, ps) bind BindingPartnerPattern(q, qs)
}