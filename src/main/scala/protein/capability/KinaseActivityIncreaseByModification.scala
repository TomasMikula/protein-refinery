package protein.capability

import protein.mechanism.{Protein, ProteinModifications}

case class KinaseActivityIncreaseByModification(kinase: Protein, mods: ProteinModifications)
