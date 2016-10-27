package proteinrefinery.lib

import nutcracker.{Defer, Propagation}
import proteinrefinery.Cost
import proteinrefinery.util.Tracking

import scala.language.higherKinds

class Lib[M[_]](implicit D: Defer[M, Cost], P: Propagation[M], T: Tracking[M]) extends
  Nuggets[M] with
  Assoc.Search[M] with
  Phosphorylation.Search[M] with
  PositiveInfluenceOnRule.Search[M] with
  PositiveInfluenceOnState.Search[M] with
  PositiveInfluenceOnPhosphorylatedStateSearch[M] with
  PositiveInfluenceOnKinaseActivity.Search[M] with
  PositiveInfluenceOnPhosphorylation.Search[M] with
  NegativeInfluenceOnAssociation.Search[M] with
  NegativeInfluenceOnPhosphorylation.Search[M]
{
  def Nuggets: Nuggets[M] = this
  def AssocSearch: Assoc.Search[M] = this
  def PhosphorylationSearch: Phosphorylation.Search[M] = this
  def NegativeInfluenceOnAssociationSearch: NegativeInfluenceOnAssociation.Search[M] = this

  implicit def Defer: Defer[M, Cost] = D
  implicit def Tracking: Tracking[M] = T
  implicit def Propagation: Propagation[M] = P
}