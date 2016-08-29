package proteinrefinery.lib

case class ModifiedProtein(p: Protein, mods: AdmissibleProteinModifications) {
  override def toString = s"$p$mods"
}
