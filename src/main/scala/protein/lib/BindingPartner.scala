package protein.lib

case class BindingPartner(p: ModifiedProtein, s: Site)

object BindingPartner {
  def apply(p: Protein, mods: ProteinModifications, s: Site): BindingPartner = BindingPartner(ModifiedProtein(p, mods), s)
}
