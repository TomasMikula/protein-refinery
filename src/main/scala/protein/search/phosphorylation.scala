package protein.search

import algebra.std.set._
import nutcracker._
import nutcracker.PropagationLang._
import nutcracker.util.free.FreeK
import protein.{mechanism, KB, Vocabulary}
import protein.mechanism.{CompetitiveBinding, Protein, Site}

case class Phosphorylation(
  assoc: Assoc, // left end is the kinase, right end is the substrate
  phosphoSite: DomRef[Site, Set[Site]] // substrate's site being phosphorylated
)

case class PhosphorylationSearch(kb: KB) {

  def search(kinase: Protein, substrate: Protein): FreeK[Vocabulary, Promised[mechanism.Phosphorylation]] =
    search0(kinase, substrate) >>= { fetch(_) }

  def search0(kinase: Protein, substrate: Protein): FreeK[Vocabulary, Phosphorylation] = {
    // XXX this version is quite primitive and cannot infer much beyond what is already given by the knowledge base,
    // except for finding indirect enzyme-substrate associations. In the future, we would like it to be able to hypothesize
    // phosphorylation at site s, if, e.g., s is a Serine and kinase is a Serine kinase.
    // Should be easy to achieve by having phosphoSites not as an atomic query, but as a search on top of more basic facts.

    for {
      // look for association
      assoc <- AssocSearch(kb).search0(kinase, substrate)
      // initialize the phosphorylated site to all sites on this substrate that kinase can phosphorylate
      phosphoSite <- variable[Site].oneOf(kb.phosphoSites(kinase, substrate):_*)
      // make sure the binding site on substrate is different from the phosphorylated site
      _ <- different(assoc.rightEnd.toLeft, phosphoSite)
    } yield Phosphorylation(assoc, phosphoSite)
  }

  def fetch(ph: Phosphorylation): FreeK[Vocabulary, Promised[mechanism.Phosphorylation]] = for {
    pr <- promiseF[mechanism.Phosphorylation].inject[Vocabulary]
    _ <- AssocSearch(kb).whenComplete(ph.assoc) { assoc =>
      whenResolvedF(ph.phosphoSite) { s =>
        completeF(pr, mechanism.Phosphorylation(assoc, s)).inject[Vocabulary]
      }
    }
  } yield pr

  def negativeInfluence(p: Protein, ph: Phosphorylation): FreeK[Vocabulary, Promised[CompetitiveBinding]] = {
    // currently the only way a protein can have negative influence on phosphorylation
    // is via negative influence on the association of enzyme and substrate
    AssocSearch(kb).negativeInfluence(p, ph.assoc)
  }
}