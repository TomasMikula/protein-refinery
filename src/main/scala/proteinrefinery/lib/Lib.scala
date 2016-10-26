package proteinrefinery.lib

object Lib extends
  Nuggets with
  Assoc.Search with
  Phosphorylation.Search with
  PositiveInfluenceOnRule.Search with
  PositiveInfluenceOnState.Search with
  PositiveInfluenceOnPhosphorylatedStateSearch with
  PositiveInfluenceOnKinaseActivity.Search with
  PositiveInfluenceOnPhosphorylation.Search with
  NegativeInfluenceOnAssociation.Search with
  NegativeInfluenceOnPhosphorylation.Search
{
  def Nuggets: Nuggets = this
  def AssocSearch: Assoc.Search = this
  def PhosphorylationSearch: Phosphorylation.Search = this
  def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search = this
}