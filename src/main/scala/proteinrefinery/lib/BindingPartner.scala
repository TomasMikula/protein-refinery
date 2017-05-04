package proteinrefinery.lib

case class BindingPartner[Ref[_]](p: ProteinPattern[Ref], s: SiteLabel)

object BindingPartner {
  def apply[Ref[_]](p: Protein, mods: ProteinModifications[Ref], s: SiteLabel): BindingPartner[Ref] =
    BindingPartner(ProteinPattern(p, mods), s)
}
