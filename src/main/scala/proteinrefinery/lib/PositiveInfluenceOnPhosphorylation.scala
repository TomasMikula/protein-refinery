package proteinrefinery.lib

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.ContF
import KB._
import proteinrefinery._

sealed trait PositiveInfluenceOnPhosphorylation {
  def agent: Protein
  def phosphorylation: Phosphorylation
}

object PositiveInfluenceOnPhosphorylation {

  /** Agent that acts as the kinase in a phosphorylation
    * is considered to have positive influence on that phosphorylation.
    */
  final case class IsKinase(phosphorylation: Phosphorylation) extends PositiveInfluenceOnPhosphorylation {
    def agent: Protein = phosphorylation.kinase
  }

  /** Agent that is part of the scaffold connecting kinase to substrate
    * is considered to have positive influence on the phosphorylation.
    */
  final case class InScaffold(agent: Protein, phosphorylation: Phosphorylation) extends PositiveInfluenceOnPhosphorylation

  /** Agent that has positive influence on kinase activity of the kinase involved in phosphorylation
    * is considered to have positive influence on that phosphorylation.
    */
  final case class ActivatesKinase(activation: PositiveInfluenceOnKinaseActivity, phosphorylation: Phosphorylation) extends PositiveInfluenceOnPhosphorylation {
    def agent: Protein = activation.agent
  }


  def search(p: Protein, ph: Phosphorylation): Prg[IncSetRef[PositiveInfluenceOnPhosphorylation]] =
    IncSet.collect(searchC(p, ph))

  def searchC(p: Protein, ph: Phosphorylation): ContF[DSL, PositiveInfluenceOnPhosphorylation] = {

    // we can immediately tell whether `p` is the kinase or part of the scaffold in `ph`
    val immediate: List[ContF[DSL, PositiveInfluenceOnPhosphorylation]] = {
      val isKinase = if(ph.kinase == p) List(IsKinase(ph)) else Nil
      val inScaffold = if(ph.assoc.bindings.tail.exists(_.left == p)) List(InScaffold(p, ph)) else Nil
      (isKinase ++ inScaffold).map(ContF.point[DSL, PositiveInfluenceOnPhosphorylation])
    }

    val indirect: ContF[DSL, PositiveInfluenceOnPhosphorylation] = PositiveInfluenceOnKinaseActivity.searchC(p, ph.kinase).map(infl => ActivatesKinase(infl, ph))

    ContF.sequence(indirect :: immediate)
  }
}

object PositiveInfluenceOnPhosphorylatedState {
  def searchC(agent: Protein, target: Protein): ContF[DSL, PositiveInfluenceOnState] =
    phosphoSitesC[DSL](target).flatMap(site => {
      val pat = ProteinPattern(target).addModification(site, SiteState("p")).get // XXX
      PositiveInfluenceOnState.searchC(agent, pat)
    })
}