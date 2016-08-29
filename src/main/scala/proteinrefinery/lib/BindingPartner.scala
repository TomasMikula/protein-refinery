package proteinrefinery.lib

case class BindingPartner(p: ModifiedProtein, s: Site)

object BindingPartner {
  def apply(p: Protein, mods: AdmissibleProteinModifications, s: Site): BindingPartner = BindingPartner(ModifiedProtein(p, mods), s)
}
