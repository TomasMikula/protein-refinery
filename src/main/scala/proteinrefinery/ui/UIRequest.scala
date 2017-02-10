package proteinrefinery.ui

import nutcracker.{Discrete, IncSet}
import proteinrefinery.lib.{PhosphoTarget, Protein, ProteinPattern, SiteLabel}

sealed abstract class UIRequest[Ref[_]]

final case class ReqGoalAssoc[Ref[_]](p: Protein, q: Protein) extends UIRequest[Ref]
final case class ReqGoalPhos[Ref[_]](kinase: Protein, substrate: Protein) extends UIRequest[Ref]
final case class ReqGoalPhosNegInfl[Ref[_]](agent: Protein, phosGoal: Ref[IncSet[Ref[Discrete[PhosphoTarget[Ref]]]]], phosDesc: String) extends UIRequest[Ref]

final case class ReqAssertBind[Ref[_]](p: Protein, ps: SiteLabel, q: Protein, qs: SiteLabel) extends UIRequest[Ref]
final case class ReqAssertKinaseActivity[Ref[_]](pp: ProteinPattern[Ref]) extends UIRequest[Ref]
final case class ReqAssertPhosSite[Ref[_]](k: Protein, s: Protein, ss: SiteLabel) extends UIRequest[Ref]