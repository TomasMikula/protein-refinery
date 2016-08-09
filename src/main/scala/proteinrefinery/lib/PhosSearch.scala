package proteinrefinery.lib

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.ContF
import KB._
import proteinrefinery._
import proteinrefinery.util.syntax._

import scalaz.Show

object PhosSearch {

  def search(kinase: Protein, substrate: Protein): Prg[IncSetRef[Phosphorylation]] =
    IncSet.collect(searchC(kinase, substrate))

  def searchC(kinase: Protein, substrate: Protein): ContF[DSL, Phosphorylation] = {
    phosphoSitesC[DSL](kinase, substrate).flatMap(s => searchC(kinase, substrate, s))
  }

  def searchC(kinase: Protein, substrate: Protein, s: Site): ContF[DSL, Phosphorylation] = {
    // XXX this version is quite primitive and cannot infer much beyond what is already given by the knowledge base,
    // except for finding indirect enzyme-substrate associations. In the future, we would like it to be able to hypothesize
    // phosphorylation at site s, if, e.g., s is a Serine and kinase is a Serine kinase.
    // Should be easy to achieve by having phosphoSites not as an atomic query, but as a search on top of more basic facts.
    for {
      a <- Assoc.searchC(kinase, substrate)
      ph <- if(s != a.bindings.last.rightS) ContF.point[DSL, Phosphorylation](Phosphorylation(a, s))
            else                            ContF.noop[DSL, Phosphorylation]
    } yield ph
  }

  def negativeInfluence(p: Protein, ph: Phosphorylation): Prg[IncSetRef[NegativeInfluenceOnPhosphorylation]] = {
    IncSet.collect(negativeInfluenceC(p, ph))
  }

  def negativeInfluenceC(p: Protein, ph: Phosphorylation): ContF[DSL, NegativeInfluenceOnPhosphorylation] = {
    // currently the only way a protein can have negative influence on phosphorylation
    // is via negative influence on the association of enzyme and substrate
    NegativeInfluenceOnAssociation.searchC(p, ph.assoc).map(NegativeInfluenceOnPhosphorylation.byNegativeInfluenceOnAssociation)
  }
}

sealed trait NegativeInfluenceOnPhosphorylation

object NegativeInfluenceOnPhosphorylation {

  final case class ByNegativeInfluenceOnAssociation(value: NegativeInfluenceOnAssociation) extends NegativeInfluenceOnPhosphorylation
  def byNegativeInfluenceOnAssociation(ni: NegativeInfluenceOnAssociation): NegativeInfluenceOnPhosphorylation = ByNegativeInfluenceOnAssociation(ni)
  def byCompetitiveBinding(cb: CompetitiveBinding): NegativeInfluenceOnPhosphorylation = byNegativeInfluenceOnAssociation(NegativeInfluenceOnAssociation.byCompetitiveBinding(cb))

  implicit def showInstance: Show[NegativeInfluenceOnPhosphorylation] = new Show[NegativeInfluenceOnPhosphorylation] {
    override def shows(x: NegativeInfluenceOnPhosphorylation): String = x match {
      case ByNegativeInfluenceOnAssociation(ni) => Show[NegativeInfluenceOnAssociation].shows(ni)
    }
  }
}

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

sealed trait PositiveInfluenceOnKinaseActivity {
  def agent: Protein
  def kinase: Protein
}

object PositiveInfluenceOnKinaseActivity {
  final case class PositiveInfluenceOnActiveState(infl: PositiveInfluenceOnState) extends PositiveInfluenceOnKinaseActivity {
    def agent: Protein = infl.agent
    def kinase: Protein = infl.target.protein
  }

  def searchC(agent: Protein, kinase: Protein): ContF[DSL, PositiveInfluenceOnKinaseActivity] =
    kinaseActivityC[DSL](kinase).flatMap(PositiveInfluenceOnState.searchC(agent, _).map(PositiveInfluenceOnActiveState(_)))
}

sealed trait PositiveInfluenceOnState {
  def agent: Protein
  def target: ProteinPattern
}

object PositiveInfluenceOnState {
  final case class ByRule(influenceOnEnablingRule: PositiveInfluenceOnRule, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent = influenceOnEnablingRule.agent
  }
  final case class ByPhosphorylation(infl: PositiveInfluenceOnPhosphorylation, target: ProteinPattern) extends PositiveInfluenceOnState {
    def agent: Protein = infl.agent
  }

  def searchC(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] =
    ContF.sequence(searchByRule(agent, target), searchByPhosphorylation(agent, target))

  private def searchByRule(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] = {
    val ap = AgentsPattern.empty.addAgent(target)._1
    for {
      r <- ContF.filter(rulesC[DSL])(r => r enables ap)
      infl <- PositiveInfluenceOnRule.searchC(agent, r)
    } yield ByRule(infl, target)
  }

  private def searchByPhosphorylation(agent: Protein, target: ProteinPattern): ContF[DSL, PositiveInfluenceOnState] = {
    val conts = target.mods.mods.iterator.mapFilter({ case (site, state) =>
      if (state.label == "p") Some(site) // XXX
      else None
    }).map[ContF[DSL, PositiveInfluenceOnState]](site =>
      for {
        k <- kinasesOfC[DSL](target.protein, site)
        ph <- PhosSearch.searchC(k, target.protein, site)
        infl <- PositiveInfluenceOnPhosphorylation.searchC(agent, ph)
      } yield ByPhosphorylation(infl, target)
    ).toList
    ContF.sequence(conts)
  }
}

object PositiveInfluenceOnPhosphorylatedState {
  def searchC(agent: Protein, target: Protein): ContF[DSL, PositiveInfluenceOnState] =
    phosphoSitesC[DSL](target).flatMap(site => {
      val pat = ProteinPattern(target).addModification(site, SiteState("p")).get // XXX
      PositiveInfluenceOnState.searchC(agent, pat)
    })
}

sealed trait PositiveInfluenceOnRule {
  def agent: Protein
  def rule: Rule
}

object PositiveInfluenceOnRule {
  final case class InLhs(agent: Protein, rule: Rule) extends PositiveInfluenceOnRule
  final case class Indirect(influenceOnEnablingRule: PositiveInfluenceOnRule, rule: Rule) extends PositiveInfluenceOnRule {
    def agent = influenceOnEnablingRule.agent
  }

  def search(agent: Protein, rule: Rule): Prg[IncSetRef[PositiveInfluenceOnRule]] =
    IncSet.collect(searchC(agent, rule))

  def searchC(agent: Protein, rule: Rule): ContF[DSL, PositiveInfluenceOnRule] =
    searchC(agent, rule, List(rule))

  private def searchC(agent: Protein, r: Rule, avoid: List[Rule]): ContF[DSL, PositiveInfluenceOnRule] = {
    val inLhs: Option[PositiveInfluenceOnRule] = if(r.lhs.agentIterator.exists(_.protein == agent)) Some(InLhs(agent, r)) else None
    val indirect: ContF[DSL, PositiveInfluenceOnRule] = rulesC[DSL].flatMap(q => { // TODO: penalize
      if(!avoid.contains(q) && (q enables r)) searchC(agent, q, q :: avoid).map(posInfl => Indirect(posInfl, r))
      else ContF.noop[DSL, PositiveInfluenceOnRule]
    })

    inLhs match {
      case Some(inLhs) => ContF.sequence(ContF.point(inLhs), indirect)
      case None => indirect
    }
  }
}