package protein.capability

import protein.mechanism.{ProteinModifications, Protein}

case class ModifiedProtein(p: Protein, mods: ProteinModifications) {
  override def toString = s"$p$mods"
}
