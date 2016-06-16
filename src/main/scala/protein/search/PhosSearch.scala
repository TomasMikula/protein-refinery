package protein.search

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.{ContF}
import protein.{Cont, DSL, Prg}
import protein.KBLang._
import protein.mechanism.{CompetitiveBinding, Phosphorylation, Protein}

object PhosSearch {

  def search(kinase: Protein, substrate: Protein): Prg[IncSetRef[Phosphorylation]] =
    search0(kinase, substrate) >>= (IncSet.collect(_))

  def searchC(kinase: Protein, substrate: Protein): Cont[Phosphorylation] =
    Cont.wrapEffect(search0(kinase, substrate))

  private def search0(kinase: Protein, substrate: Protein): Prg[Cont[Phosphorylation]] = {
    // XXX this version is quite primitive and cannot infer much beyond what is already given by the knowledge base,
    // except for finding indirect enzyme-substrate associations. In the future, we would like it to be able to hypothesize
    // phosphorylation at site s, if, e.g., s is a Serine and kinase is a Serine kinase.
    // Should be easy to achieve by having phosphoSites not as an atomic query, but as a search on top of more basic facts.
    for {
      sites <- phosphoSitesS[DSL](kinase, substrate)
      assocs <- AssocSearch.search(kinase, substrate)
      pairs = ContF.tuple2(IncSet.forEach(sites), IncSet.forEach(assocs))
    } yield ContF.filterMap(pairs)({ case (s, a) =>
      if (s != a.bindings.last.rightS) Some(Phosphorylation(a, s))
      else None
    })
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