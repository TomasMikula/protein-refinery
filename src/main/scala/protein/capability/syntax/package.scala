package protein.capability

import scala.language.implicitConversions
import protein.capability.AgentsPattern._
import protein.mechanism.{Binding, Protein, ProteinModifications, Site, SiteState}

import scalaz.{NonEmptyList, State}

package object syntax {

  implicit class SymbolOps(sym: Symbol) {

    def apply(ss: (Site, SiteState)*): ProteinPattern =
      ProteinPattern(Protein(sym), ProteinModifications(ss.toMap))

    def @@ (s: Site): BindingPartnerPattern = BindingPartnerPattern(
      ProteinPattern(Protein(sym), ProteinModifications.noModifications),
      s
    )

    def ~(s: SiteState): (Site, SiteState) = (Site(sym.name), s)

  }

  implicit class ProteinPatternOps(p: ProteinPattern) {

    def @@ (s: Site): BindingPartnerPattern = BindingPartnerPattern(p, s)

  }

  implicit class BindingPartnerPatternOps(bp: BindingPartnerPattern) {
    def binds(that: BindingPartnerPattern): Binding = (for {
      i <- addAgent(bp.p)
      j <- addAgent(that.p)
      lhs <- State.get[AgentsPattern]
      a = Link(i, bp.s, j, that.s)
    } yield Binding(Rule(lhs, NonEmptyList(a)), i, j, bp.s, that.s)).eval(AgentsPattern.empty)
  }

  implicit def symbolToProtein(sym: Symbol): Protein = Protein(sym)

  implicit def symbolToSite(sym: Symbol): Site = Site(sym.name)

  implicit def stringToState(s: String): SiteState = SiteState(s)


  // examples
  'A@@'x binds 'B@@'y
  'A('x~"p")
  'A('x~"p")@@'z
  'A('x~"p")@@'z binds 'B('y~"u")@@'w
}
