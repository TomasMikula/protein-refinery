package protein

package object syntax {
  implicit class Protein(name: Symbol)
  implicit class Site(name: String)
  implicit class SiteState(label: String)

  case class BindingPartner(p: Protein, s: Option[Site], cond: List[(Site, SiteState)])
  case class Binding(x: BindingPartner, y: BindingPartner)

  case class Phosphorylation(
    kinase: Protein,
    substrate: Protein,
    phosphoSite: Option[Site]
  )

  implicit class SymbolOps(symbol: Symbol) {

  }

  implicit class ProteinOps(p: Protein) {

  }

  implicit class BindingPartnerOps(x: BindingPartner) {

  }

}
