package protein.capability

import protein.capability.AgentsPattern._
import protein.mechanism.{Binding, Protein, Site}

import scalaz.{State}

case class BindingPartnerPattern(p: ProteinPattern, s: Site) {
  def overlaps(that: BindingPartnerPattern): Boolean = (this.s == that.s) && (this.p isCompatibleWith that.p)

  def bind(that: BindingPartnerPattern): Binding = (for {
    i <- addAgent(this.p)
    j <- addAgent(that.p)
    _ <- requireUnbound(i, this.s)
    _ <- requireUnbound(j, that.s)
    lhs <- State.get[AgentsPattern]
    a = Link(i, this.s, j, that.s)
  } yield Binding(Rule(lhs, List(a)), i, j, this.s, that.s)).eval(AgentsPattern.empty)
}

object BindingPartnerPattern {
  def apply(p: Protein, s: Site): BindingPartnerPattern =
    BindingPartnerPattern(ProteinPattern(p), s)
}