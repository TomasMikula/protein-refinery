package proteinrefinery.ui

import nutcracker.{Antichain, DRef, IncSet}
import proteinrefinery.lib.{PhosphoTarget, Protein, ProteinPattern, SiteLabel}

sealed abstract class UIRequest

final case class ReqGoalAssoc(p: Protein, q: Protein) extends UIRequest
final case class ReqGoalPhos(kinase: Protein, substrate: Protein) extends UIRequest
final case class ReqGoalPhosNegInfl(agent: Protein, phosGoal: DRef[IncSet[DRef[Antichain[PhosphoTarget[DRef]]]]], phosDesc: String) extends UIRequest

final case class ReqAssertBind(p: Protein, ps: SiteLabel, q: Protein, qs: SiteLabel) extends UIRequest
final case class ReqAssertKinaseActivity(pp: ProteinPattern[DRef]) extends UIRequest
final case class ReqAssertPhosSite(k: Protein, s: Protein, ss: SiteLabel) extends UIRequest