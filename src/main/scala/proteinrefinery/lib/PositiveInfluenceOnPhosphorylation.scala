package proteinrefinery.lib

import nutcracker.Antichain
import nutcracker.util.ContF
import proteinrefinery._

sealed trait PositiveInfluenceOnPhosphorylation {
  def agent: Protein
  def phosphorylation: Phosphorylation
}

object PositiveInfluenceOnPhosphorylation {

  type Ref = Antichain.Ref[PositiveInfluenceOnPhosphorylation]

  // Constructors

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


  // Search

  def searchC(p: Protein, ph: Phosphorylation): ContF[DSL, Ref] = {

    // we can immediately tell whether `p` is the kinase or part of the scaffold in `ph`
    val immediate: List[ContF[DSL, Ref]] = {
      val isKinase = if(ph.kinase == p) List(IsKinase(ph)) else Nil
      val inScaffold = if(ph.assoc.bindings.tail.exists(_.left == p)) List(InScaffold(p, ph)) else Nil
      (isKinase ++ inScaffold).map(Antichain.cellC[DSL, PositiveInfluenceOnPhosphorylation](_))
    }

    val indirect: ContF[DSL, Ref] = Antichain.map(PositiveInfluenceOnKinaseActivity.searchC(p, ph.kinase))(infl => ActivatesKinase(infl, ph))

    ContF.sequence(indirect :: immediate)
  }
}

object PositiveInfluenceOnPhosphorylatedState {
  def searchC(agent: Protein, target: Protein): ContF[DSL, PositiveInfluenceOnState.Ref] =
    Nuggets.phosphoSitesC[DSL](target).flatMap(site => {
      val pat = AdmissibleProteinPattern(target).addModification(site, SiteState("p")).get // XXX
      PositiveInfluenceOnState.searchC(agent, pat)
    })
}