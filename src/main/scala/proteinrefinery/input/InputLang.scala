package proteinrefinery.input

import scala.language.higherKinds
import proteinrefinery.lib.{AgentIncreasesKinaseActivity, Binding, Protein, ProteinModifications, Site}

sealed trait InputLang[A]

object InputLang {
  case class A_Binds(bnd: Binding) extends InputLang[Unit]
  case class A_PhosTarget(kinase: Protein, substrate: Protein, site: Site) extends InputLang[Unit]
  case class A_KinaseActivityIncreaseByModification(kinase: Protein, modifications: ProteinModifications) extends InputLang[Unit]
  case class Q_KinaseActivityIncrease[Var[_]](subject: Protein, kinase: Protein) extends InputLang[Var[AgentIncreasesKinaseActivity]]
}