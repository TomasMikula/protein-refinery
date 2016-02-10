package protein.capability

import scala.language.implicitConversions

import protein.mechanism.{SiteState, ProteinModifications, Protein, Site}

package object syntax {

  implicit class SymbolOps(sym: Symbol) {

    def apply(ss: (Site, SiteState)*): ModifiedProtein =
      ModifiedProtein(Protein(sym), ProteinModifications(ss.toMap))

    def @@ (s: Site): BindingPartner = BindingPartner(
      ModifiedProtein(Protein(sym), ProteinModifications.noModifications),
      s
    )

    def ~(s: SiteState): (Site, SiteState) = (Site(sym.name), s)

  }

  implicit class ModifiedProteinOps(p: ModifiedProtein) {

    def @@ (s: Site): BindingPartner = BindingPartner(p, s)

  }

  implicit class BindingPartnerOps(bp: BindingPartner) {
    def binds(that: BindingPartner): Binding = Binding(bp, that)
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
