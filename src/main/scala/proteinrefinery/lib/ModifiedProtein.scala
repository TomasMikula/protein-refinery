package proteinrefinery.lib

case class ModifiedProtein(p: Protein, mods: ProteinModifications) {
  override def toString = s"$p$mods"
}
