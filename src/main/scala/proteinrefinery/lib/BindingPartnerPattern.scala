package proteinrefinery.lib

import proteinrefinery.lib.AdmissibleAgentsPattern._
import proteinrefinery.lib.ProteinModifications.LocalSiteId

import scalaz.State

case class BindingPartnerPattern(p: AdmissibleProteinPattern, s: LocalSiteId) {
  def overlaps(that: BindingPartnerPattern): Boolean = (this.s == that.s) && (this.p isCompatibleWith that.p)

  def bind(that: BindingPartnerPattern): Binding = (for {
    i <- addAgent(this.p)
    j <- addAgent(that.p)
    _ <- requireUnbound0(i, this.s)
    _ <- requireUnbound0(j, that.s)
    lhs <- State.get[AdmissibleAgentsPattern]
    a = Link(i, this.s, j, that.s)
  } yield Binding(Rule(lhs, List(a)), i, j, this.s, that.s)).eval(AdmissibleAgentsPattern.empty)
}

object BindingPartnerPattern {
  def apply(p: Protein, s: LocalSiteId): BindingPartnerPattern =
    BindingPartnerPattern(AdmissibleProteinPattern(p), s)
}