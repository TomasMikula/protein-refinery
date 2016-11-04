package proteinrefinery

import nutcracker.{Defer, IncSets, Propagation}
import proteinrefinery.lib.{Assoc, NegativeInfluenceOnAssociation, NegativeInfluenceOnPhosphorylation, Nuggets, Phosphorylation, PositiveInfluenceOnKinaseActivity, PositiveInfluenceOnPhosphorylatedStateSearch, PositiveInfluenceOnPhosphorylation, PositiveInfluenceOnRule, PositiveInfluenceOnState, Syntax}
import proteinrefinery.util.Tracking

import scala.language.higherKinds

class Lib[M[_], Ref[_]](implicit D: Defer[M, Cost], P: Propagation[M, Ref], T: Tracking[M, Ref]) extends
  Nuggets[M, Ref] with
  Assoc.Search[M, Ref] with
  Phosphorylation.Search[M, Ref] with
  PositiveInfluenceOnRule.Search[M, Ref] with
  PositiveInfluenceOnState.Search[M, Ref] with
  PositiveInfluenceOnPhosphorylatedStateSearch[M, Ref] with
  PositiveInfluenceOnKinaseActivity.Search[M, Ref] with
  PositiveInfluenceOnPhosphorylation.Search[M, Ref] with
  NegativeInfluenceOnAssociation.Search[M, Ref] with
  NegativeInfluenceOnPhosphorylation.Search[M, Ref] with
  Syntax[Ref]
{
  def Nuggets: Nuggets[M, Ref] = this
  def AssocSearch: lib.Assoc.Search[M, Ref] = this
  def PhosphorylationSearch: lib.Phosphorylation.Search[M, Ref] = this
  def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search[M, Ref] = this
  def IncSets: IncSets[M, Ref] = new IncSets[M, Ref]()(P)

  implicit def Defer: Defer[M, Cost] = D
  implicit def Tracking: Tracking[M, Ref] = T
  implicit def Propagation: Propagation[M, Ref] = P

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
  type CompetitiveBinding = lib.CompetitiveBinding[Ref]
  type Assoc = lib.Assoc[Ref]
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
  def SiteState(label: String): SiteState = lib.SiteState(label)
  def SiteWithState(label: SiteLabel, state: SiteState): SiteWithState = lib.SiteWithState(label, state)
  def ProteinModifications(mods: (SiteLabel, SiteState)*): ProteinModifications = lib.ProteinModifications(mods:_*)
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
    def InLhs(agent: ProteinPattern, rule: Rule): PositiveInfluenceOnRule = lib.PositiveInfluenceOnRule.InLhs(agent, rule)
  }
}