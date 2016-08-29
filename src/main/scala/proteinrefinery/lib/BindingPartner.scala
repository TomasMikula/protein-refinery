package proteinrefinery.lib

case class BindingPartner(p: ProteinPattern, s: Site)

object BindingPartner {
  def apply(p: Protein, mods: AdmissibleProteinModifications, s: Site): BindingPartner = BindingPartner(ProteinPattern(p, mods), s)
}
