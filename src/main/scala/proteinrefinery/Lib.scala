package proteinrefinery

import nutcracker.util.EqualK
import nutcracker.{Defer, IncSets, Propagation}
import proteinrefinery.lib.{AgentsPattern, Assoc, NegativeInfluenceOnAssociation, NegativeInfluenceOnPhosphorylation, NegativeInfluenceOnRule, Nuggets, PhosphoTarget, Phosphorylation, PositiveInfluenceOfRuleOnRule, PositiveInfluenceOnKinaseActivity, PositiveInfluenceOnPhosphorylatedStateSearch, PositiveInfluenceOnRule, PositiveInfluenceOnState, Rule, Syntax}
import proteinrefinery.util.Tracking

import scala.language.higherKinds
import scalaz.Monad

class Lib[M[_], Ref[_], Val[_]](implicit D: Defer[M, Cost], P: Propagation[M, Ref, Val], T: Tracking[M, Ref, Val], M0: Monad[M], E: EqualK[Ref]) extends
  Nuggets[M, Ref, Val] with
  Assoc.Search[M, Ref, Val] with
  Phosphorylation.Search[M, Ref, Val] with
  PhosphoTarget.Ops[M, Ref, Val] with
  AgentsPattern.Ops[M, Ref, Val] with
  Rule.Ops[M, Ref, Val] with
  PositiveInfluenceOnRule.Search[M, Ref, Val] with
  PositiveInfluenceOfRuleOnRule.Search[M, Ref, Val] with
  PositiveInfluenceOnState.Search[M, Ref, Val] with
  PositiveInfluenceOnPhosphorylatedStateSearch[M, Ref, Val] with
  PositiveInfluenceOnKinaseActivity.Search[M, Ref, Val] with
//  PositiveInfluenceOnPhosphorylation.Search[M, Ref] with
  NegativeInfluenceOnAssociation.Search[M, Ref, Val] with
  NegativeInfluenceOnPhosphorylation.Search[M, Ref, Val] with
  NegativeInfluenceOnRule.Search[M, Ref, Val] with
  Syntax[Ref]
{
  protected implicit val M: Monad[M] = M0
  implicit val RefEquality: EqualK[Ref] = E
  protected implicit val Propagation: Propagation[M, Ref, Val] = P
  implicit val Tracking: Tracking[M, Ref, Val] = T
  implicit val Defer: Defer[M, Cost] = D

  def Nuggets: Nuggets[M, Ref, Val] = this
  def AssocSearch: lib.Assoc.Search[M, Ref, Val] = this
  def PhosphoTargetOps: lib.PhosphoTarget.Ops[M, Ref, Val] = this
  def AgentsPatternOps: lib.AgentsPattern.Ops[M, Ref, Val] = this
  def RuleOps: lib.Rule.Ops[M, Ref, Val] = this
  def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search[M, Ref, Val] = this
  def NegativeInfluenceOnRuleSearch: NegativeInfluenceOnRule.Search[M, Ref, Val] = this
  def IncSets: IncSets[M, Ref, Val] = new IncSets[M, Ref, Val]()(P)

  type Protein = lib.Protein
  type SiteLabel = lib.SiteLabel
  type SiteState = lib.SiteState.SiteState
  type ISite = lib.ISite[Ref]
  type SiteWithState = lib.SiteWithState[Ref]
  type ProteinModifications = lib.ProteinModifications[Ref]
  type ProteinPattern = lib.ProteinPattern[Ref]
  type AgentsPattern = lib.AgentsPattern[Ref]
  type Rule = lib.Rule[Ref]
  type Binding = lib.Binding[Ref]
  type BindingData = lib.BindingData[Ref]
  type CompetitiveBinding = lib.CompetitiveBinding[Ref]
  type Assoc = lib.Assoc[Ref]
  type PhosphoTriple = lib.PhosphoTriple[Ref]
  type Phosphorylation = lib.Phosphorylation[Ref]
  type NegativeInfluenceOnAssociation = lib.NegativeInfluenceOnAssociation[Ref]
  type NegativeInfluenceOnPhosphorylation = lib.NegativeInfluenceOnPhosphorylation[Ref]
  type PositiveInfluenceOnRule = lib.PositiveInfluenceOnRule[Ref]

  def Protein(name: String): Protein = lib.Protein(name)
  def Protein(sym: Symbol): Protein = lib.Protein(sym)
  def SiteLabel(label: String): SiteLabel = lib.SiteLabel(label)
  object Site {
    def fromLabel(label: SiteLabel) = lib.Site.fromLabel(label)
  }
  object ISite {
    def apply(site: lib.Site.Definite, refs: lib.Site.Ref[Ref]*): ISite = lib.ISite(site, refs:_*)
    def apply(ref: lib.Site.Ref[Ref], refs: lib.Site.Ref[Ref]*): ISite = lib.ISite(ref, refs:_*)
    def apply(s: lib.ProteinModifications.LocalSiteId[Ref]): ISite = s match {
      case lib.ProteinModifications.DefiniteLabel(s) => ISite(s)
      case lib.ProteinModifications.SiteRef(ref) => ISite(ref)
    }
  }
  def SiteState(label: String): SiteState = lib.SiteState(label)
  def SiteWithState(label: SiteLabel, state: SiteState): SiteWithState = lib.SiteWithState(label, state)
  def ProteinModifications(mods: (SiteLabel, SiteState)*): ProteinModifications = lib.ProteinModifications.from(mods:_*)
  def ProteinModifications(mods: List[SiteWithState]): ProteinModifications = lib.ProteinModifications(mods)
  def ProteinPattern(p: Protein): ProteinPattern = lib.ProteinPattern(p)
  def ProteinPattern(p: Protein, mods: ProteinModifications): ProteinPattern = lib.ProteinPattern(p, mods)
  object AgentsPattern {
    type Delta = lib.AgentsPattern.Delta[Ref]
  }
  object Rule {
    type Delta = lib.Rule.Delta[Ref]
  }
  def CompetitiveBinding(base: Binding, competing: Binding): CompetitiveBinding = lib.CompetitiveBinding(base, competing)
  def Assoc(bindings: List[Binding]): Assoc = lib.Assoc(bindings)
  def Phosphorylation(assoc: Assoc, site: SiteLabel): Phosphorylation = lib.Phosphorylation(assoc, site)
  object NegativeInfluenceOnPhosphorylation {
    def byCompetitiveBinding(b: CompetitiveBinding): NegativeInfluenceOnPhosphorylation =
      lib.NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(b)
  }
  object PositiveInfluenceOnRule {
    def InLhs(agent: ProteinPattern, rule: lib.Rule.Ref[Ref]): PositiveInfluenceOnRule = lib.PositiveInfluenceOnRule.InLhs(agent, rule)
  }
}