package proteinrefinery.lib

sealed trait AgentIncreasesKinaseActivity {
  val subject: Protein
  val kinase: Protein
}

case class AgentIncreasesKinaseActivityByModification(
  subject: Protein,
  kinase: Protein,
  modificationIncrease: AgentIncreasesModification,
  activityIncrease: KinaseActivityIncreaseByModification
) extends AgentIncreasesKinaseActivity