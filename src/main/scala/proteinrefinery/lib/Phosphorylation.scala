package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Antichain, IncSet}
import nutcracker.IncSet._
import nutcracker.util.ContU

import scalaz.{Monad, Show}

case class Phosphorylation(
  assoc: Assoc,
  phosphoSite: SiteLabel
) {
  def kinase: Protein = assoc.bindings.head.left

  override def toString = s"${assoc.bindings.head.left} phosphorylates ${assoc.bindings.last.right} at $phosphoSite (via $assoc)"
}

object Phosphorylation {

  type Ref = Antichain.Ref[Phosphorylation]

  trait Search[M[_]] {
    implicit def Propagation: nutcracker.Propagation[M]
    implicit def Tracking: proteinrefinery.util.Tracking[M]

    def Nuggets: proteinrefinery.lib.Nuggets[M]
    def AssocSearch: Assoc.Search[M]

    def phosphorylation(kinase: Protein, substrate: Protein)(implicit M: Monad[M]): M[IncSetRef[Ref]] =
      IncSet.collect(phosphorylationC(kinase, substrate))

    def phosphorylationC(kinase: Protein, substrate: Protein)(implicit M: Monad[M]): ContU[M, Ref] = {
      Nuggets.phosphoSitesC(kinase, substrate).flatMap(s => phosphorylationC(kinase, substrate, s))
    }

    def phosphorylationC(kinase: Protein, substrate: Protein, s: SiteLabel)(implicit M: Monad[M]): ContU[M, Ref] = {
      // XXX this version is quite primitive and cannot infer much beyond what is already given by the knowledge base,
      // except for finding indirect enzyme-substrate associations. In the future, we would like it to be able to hypothesize
      // phosphorylation at site s, if, e.g., s is a Serine and kinase is a Serine kinase.
      // Should be easy to achieve by having phosphoSites not as an atomic query, but as a search on top of more basic facts.
      Antichain.filterMap(AssocSearch.assocC(kinase, substrate)) { a =>
        if (s != a.bindings.last.rightS) Some(Phosphorylation(a, s))
        else None
      }
    }

  }


  // Typeclass instances

  implicit def showInstance: Show[Phosphorylation] = new Show[Phosphorylation] {
    override def shows(p: Phosphorylation): String = p.toString
  }
}