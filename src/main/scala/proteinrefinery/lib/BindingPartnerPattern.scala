package proteinrefinery.lib

import proteinrefinery.lib.AdmissibleAgentsPattern._
import proteinrefinery.lib.ProteinModifications.LocalSiteId

import scalaz.State
import scalaz.std.either._
import scalaz.std.option._
import scalaz.syntax.equal._

case class BindingPartnerPattern(p: ProteinPattern, s: LocalSiteId) {
  def overlaps(that: BindingPartnerPattern): Boolean = (this.s === that.s) && (this.p isCompatibleWith that.p)

  def bind(that: BindingPartnerPattern): Option[Binding] = (for {
    i <- addAgentOpt(this.p)
    j <- addAgentOpt(that.p)
    _ <- requireUnbound0(i, this.s).lift[Option]
    _ <- requireUnbound0(j, that.s).lift[Option]
    lhs <- State.get[AdmissibleAgentsPattern].lift[Option]
    a = Link(i, this.s, j, that.s)
  } yield Binding(AdmissibleRule(lhs, List(a)), i, j, this.s, that.s)).eval(AdmissibleAgentsPattern.empty)
}

object BindingPartnerPattern {
  def apply(p: Protein, s: LocalSiteId): BindingPartnerPattern =
    BindingPartnerPattern(AdmissibleProteinPattern(p), s)
}