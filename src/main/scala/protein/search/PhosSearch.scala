package protein.search

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.{ContF}
import protein.{Cont, DSL, Prg}
import protein.KBLang._
import protein.mechanism.{CompetitiveBinding, Phosphorylation, Protein, Site}

object PhosSearch {

  def search(kinase: Protein, substrate: Protein): Prg[IncSetRef[Phosphorylation]] =
    IncSet.collect(searchC(kinase, substrate))

  def searchC(kinase: Protein, substrate: Protein): Cont[Phosphorylation] = {
    phosphoSitesC[DSL](kinase, substrate).flatMap(s => searchC(kinase, substrate, s))
  }

  def searchC(kinase: Protein, substrate: Protein, s: Site): Cont[Phosphorylation] = {
    // XXX this version is quite primitive and cannot infer much beyond what is already given by the knowledge base,
    // except for finding indirect enzyme-substrate associations. In the future, we would like it to be able to hypothesize
    // phosphorylation at site s, if, e.g., s is a Serine and kinase is a Serine kinase.
    // Should be easy to achieve by having phosphoSites not as an atomic query, but as a search on top of more basic facts.
    for {
      a <- AssocSearch.searchC(kinase, substrate)
      ph <- if(s != a.bindings.last.rightS) ContF.point[DSL, Phosphorylation](Phosphorylation(a, s))
            else                            ContF.noop[DSL, Phosphorylation]
    } yield ph
  }

  def negativeInfluence(p: Protein, ph: Phosphorylation): Prg[IncSetRef[CompetitiveBinding]] = {
    IncSet.collect(negativeInfluenceC(p, ph))
  }

  def negativeInfluenceC(p: Protein, ph: Phosphorylation): Cont[CompetitiveBinding] = {
    // currently the only way a protein can have negative influence on phosphorylation
    // is via negative influence on the association of enzyme and substrate
    AssocSearch.negativeInfluenceC(p, ph.assoc)
  }
}
