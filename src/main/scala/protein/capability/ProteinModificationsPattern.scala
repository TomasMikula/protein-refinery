package protein.capability

import protein.mechanism.{Protein, ProteinModifications}

case class ProteinPattern(protein: Protein, mods: ProteinModifications) {
  def isCompatibleWith(that: ProteinPattern): Boolean =
    (this.protein == that.protein) && (this.mods combine that.mods).isDefined
}
