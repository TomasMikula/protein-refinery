package proteinrefinery.ui

import scala.language.existentials
import nutcracker.{Antichain, DRef}
import nutcracker.IncSet.IncSetRef
import proteinrefinery.lib.{Phosphorylation, Protein, ProteinPattern, Site}

sealed abstract class UIRequest

final case class ReqGoalAssoc(p: Protein, q: Protein) extends UIRequest
final case class ReqGoalPhos(kinase: Protein, substrate: Protein) extends UIRequest
final case class ReqGoalPhosNegInfl(agent: Protein, phosGoal: IncSetRef[_ <: DRef[Antichain[Phosphorylation]]], phosDesc: String) extends UIRequest

final case class ReqAssertBind(p: Protein, ps: Site, q: Protein, qs: Site) extends UIRequest
final case class ReqAssertKinaseActivity(pp: ProteinPattern) extends UIRequest
final case class ReqAssertPhosSite(k: Protein, s: Protein, ss: Site) extends UIRequest