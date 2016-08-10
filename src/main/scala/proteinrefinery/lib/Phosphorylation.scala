package proteinrefinery.lib

import nutcracker.IncSet
import nutcracker.IncSet._
import nutcracker.util.{ContF, _}
import proteinrefinery._

import scalaz.Show

case class Phosphorylation(
  assoc: Assoc,
  phosphoSite: Site
) {
  def kinase: Protein = assoc.bindings.head.left

  override def toString = s"${assoc.bindings.head.left} phosphorylates ${assoc.bindings.last.right} at $phosphoSite (via $assoc)"
}

object Phosphorylation {

  // Search

  def search(kinase: Protein, substrate: Protein): Prg[IncSetRef[Phosphorylation]] =
    IncSet.collect(searchC(kinase, substrate))

  def searchC(kinase: Protein, substrate: Protein): ContF[DSL, Phosphorylation] = {
    KB.phosphoSitesC[DSL](kinase, substrate).flatMap(s => searchC(kinase, substrate, s))
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


  // Typeclass instances

  implicit def showInstance: Show[Phosphorylation] = new Show[Phosphorylation] {
    override def shows(p: Phosphorylation): String = p.toString
  }
}