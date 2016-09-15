package proteinrefinery.lib

case class BindingPartner(p: ProteinPattern, s: SiteLabel)

object BindingPartner {
  def apply(p: Protein, mods: AdmissibleProteinModifications, s: SiteLabel): BindingPartner = BindingPartner(ProteinPattern(p, mods), s)
}
