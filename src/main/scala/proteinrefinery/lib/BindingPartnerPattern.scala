package proteinrefinery.lib

import scala.language.higherKinds

import nutcracker.util.EqualK
import nutcracker.util.EqualK._
import proteinrefinery.lib.AgentsPattern._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import scalaz.State
import scalaz.std.either._
import scalaz.syntax.equal._

case class BindingPartnerPattern[Ref[_]](p: ProteinPattern[Ref], s: LocalSiteId[Ref]) {
  def overlaps(that: BindingPartnerPattern[Ref])(implicit ev: EqualK[Ref]): Boolean =
    (this.s === that.s) && (this.p isCompatibleWith that.p)

  def bind(that: BindingPartnerPattern[Ref]): Binding[Ref] = (for {
    i <- addAgent(this.p)
    j <- addAgent(that.p)
    _ <- requireUnbound0(i, this.s)
    _ <- requireUnbound0(j, that.s)
    lhs <- State.get[AgentsPattern[Ref]]
    a = Link(i, this.s, j, that.s)
  } yield Binding(Rule(lhs, List(a)), i, j, this.s, that.s)).eval(AgentsPattern.empty)
}

object BindingPartnerPattern {
  def apply[Ref[_]](p: Protein, s: LocalSiteId[Ref]): BindingPartnerPattern[Ref] =
    BindingPartnerPattern(ProteinPattern[Ref](p), s)
}