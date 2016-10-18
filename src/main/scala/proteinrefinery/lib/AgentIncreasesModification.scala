package proteinrefinery.lib

import scalaz.NonEmptyList

final case class AgentIncreasesModification(
  subject: Protein,
  target: Protein,
  targetMods: ProteinModifications,
  rules: NonEmptyList[Rule]
)