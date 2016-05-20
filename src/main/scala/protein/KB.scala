package protein

import scala.language.higherKinds
import nutcracker.util.~>>
import protein.KBLang._
import protein.mechanism.{Binding, Protein, ProteinModifications, Site}

import scalaz.Reader

/**
  * Knowledge base.
  */
trait KB {

  def sitesOf(p: Protein): Seq[Site]

  def neighborsOf(p: Protein): Seq[Binding]

  def phosphoSites(kinase: Protein, substrate: Protein): Seq[Site]

  def modsIncreasingKinaseActivity(kinase: Protein): Seq[ProteinModifications]

}

object KB {
  implicit def interpreter: KBLangK ~>> Reader[KB, ?] = new (KBLangK ~>> Reader[KB, ?]) {
    def apply[K[_], A](f: KBLangK[K, A]): Reader[KB, A] = Reader(kb => f match {
      case SitesOf(p) => kb.sitesOf(p)
      case BindingsOf(p) => kb.neighborsOf(p)
      case PhosphoSites(k, s) => kb.phosphoSites(k, s)
    })
  }
}