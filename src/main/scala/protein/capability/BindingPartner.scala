package protein.capability

import protein.mechanism.{ProteinModifications, Protein, Site}

case class BindingPartner(p: ModifiedProtein, s: Site)

object BindingPartner {
  def apply(p: Protein, mods: ProteinModifications, s: Site): BindingPartner = BindingPartner(ModifiedProtein(p, mods), s)
}
