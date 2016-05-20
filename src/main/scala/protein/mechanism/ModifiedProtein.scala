package protein.mechanism

case class ModifiedProtein(p: Protein, mods: ProteinModifications) {
  override def toString = s"$p$mods"
}
