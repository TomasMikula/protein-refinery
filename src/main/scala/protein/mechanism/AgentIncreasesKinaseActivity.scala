package protein.mechanism

import protein.capability.KinaseActivityIncreaseByModification

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