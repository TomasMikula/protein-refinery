package protein

import scala.language.higherKinds
import nutcracker.util.{Lst, Step, WriterState}
import protein.KBLang._
import protein.mechanism.{Binding, Protein, ProteinModifications, Site}

import scalaz.Reader

/**
  * Knowledge base.
  *
  * @tparam K type of callbacks that are executed for query results.
  */
trait KB[K[_]] {

  def sitesOf(p: Protein)(f: Site => K[Unit]): (Lst[K[Unit]], KB[K], Unit)

  def neighborsOf(p: Protein)(f: Binding => K[Unit]): (Lst[K[Unit]], KB[K], Unit)

  def phosphoSites(kinase: Protein, substrate: Protein)(f: Site => K[Unit]): (Lst[K[Unit]], KB[K], Unit)

  def modsIncreasingKinaseActivity(kinase: Protein)(f: ProteinModifications => K[Unit]): (Lst[K[Unit]], KB[K], Unit)

}

object KB {
  implicit def interpreter: Step[KBLang, KB] = new Step[KBLang, KB] {
    def apply[K[_], A](op: KBLang[K, A]): WriterState[Lst[K[Unit]], KB[K], A] = WriterState(kb => op match {
      case SitesOf(p, f) => kb.sitesOf(p)(f)
      case BindingsOf(p, f) => kb.neighborsOf(p)(f)
      case PhosphoSites(k, s, f) => kb.phosphoSites(k, s)(f)
    })
  }
}