package proteinrefinery.lib

import scala.language.implicitConversions
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteState.SiteState

package object syntax {

  implicit class SymbolOps(sym: Symbol) {

    def apply(ss: (SiteLabel, SiteState)*): ProteinPattern =
      ProteinPattern(Protein(sym), ProteinModifications(ss:_*))

    def @@ (s: SiteLabel): BindingPartnerPattern =
      BindingPartnerPattern(Protein(sym), LocalSiteId(s))

    def ~(s: SiteState): (SiteLabel, SiteState) = (SiteLabel(sym.name), s)

  }

  implicit class ProteinOps(p: Protein) {

    def apply(ss: (SiteLabel, SiteState)*): ProteinPattern =
      ProteinPattern(p, ProteinModifications(ss:_*))

    def @@ (s: SiteLabel): BindingPartnerPattern =
      BindingPartnerPattern(p, LocalSiteId(s))
  }

  implicit class ProteinPatternOps(p: ProteinPattern) {

    def @@ (s: SiteLabel): BindingPartnerPattern = BindingPartnerPattern(p, LocalSiteId(s))

  }

  implicit class BindingPartnerPatternOps(bp: BindingPartnerPattern) {
    def binds(that: BindingPartnerPattern): Binding = bp bind that
  }

  implicit def symbolToProtein(sym: Symbol): Protein = Protein(sym)

  implicit def symbolToSite(sym: Symbol): SiteLabel = SiteLabel(sym.name)

  implicit def stringToState(s: String): SiteState = SiteState(s)


  // examples
  'A@@'x binds 'B@@'y
  'A('x~"p")
  'A('x~"p")@@'z
  'A('x~"p")@@'z binds 'B('y~"u")@@'w
}
