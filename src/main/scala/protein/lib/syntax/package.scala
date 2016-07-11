package protein.capability

import scala.language.implicitConversions
import protein.lib.{Binding, BindingPartnerPattern, Protein, ProteinModifications, ProteinPattern, Site, SiteState}

package object syntax {

  implicit class SymbolOps(sym: Symbol) {

    def apply(ss: (Site, SiteState)*): ProteinPattern =
      ProteinPattern(Protein(sym), ProteinModifications(ss.toMap))

    def @@ (s: Site): BindingPartnerPattern = BindingPartnerPattern(Protein(sym), s)

    def ~(s: SiteState): (Site, SiteState) = (Site(sym.name), s)

  }

  implicit class ProteinPatternOps(p: ProteinPattern) {

    def @@ (s: Site): BindingPartnerPattern = BindingPartnerPattern(p, s)

  }

  implicit class BindingPartnerPatternOps(bp: BindingPartnerPattern) {
    def binds(that: BindingPartnerPattern): Binding = bp bind that
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
