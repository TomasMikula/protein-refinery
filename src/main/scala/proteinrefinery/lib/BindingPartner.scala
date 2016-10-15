package proteinrefinery.lib

case class BindingPartner(p: AdmissibleProteinPattern, s: SiteLabel)

object BindingPartner {
  def apply(p: Protein, mods: ProteinModifications, s: SiteLabel): BindingPartner =
    BindingPartner(AdmissibleProteinPattern(p, mods), s)
}
