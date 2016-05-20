package protein

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA}
import protein.mechanism.{Binding, Protein, Site}

import scalaz.~>

sealed trait KBLang[A]

object KBLang {
  type KBLangK[K[_], A] = KBLang[A]

  case class SitesOf(p: Protein) extends KBLang[Seq[Site]]
  case class BindingsOf(p: Protein) extends KBLang[Seq[Binding]]
  case class PhosphoSites(kinase: Protein, substrate: Protein) extends KBLang[Seq[Site]]

  def sitesOf[K[_]](p: Protein): KBLangK[K, Seq[Site]] = SitesOf(p)
  def bindingsOf[K[_]](p: Protein): KBLangK[K, Seq[Binding]] = BindingsOf(p)
  def phosphoSites[K[_]](kinase: Protein, substrate: Protein): KBLangK[K, Seq[Site]] = PhosphoSites(kinase, substrate)

  def sitesOfF(p: Protein): FreeK[KBLangK, Seq[Site]] =
    FreeK.liftF[KBLangK, Seq[Site]](sitesOf(p))
  def bindingsOfF(p: Protein): FreeK[KBLangK, Seq[Binding]] =
    FreeK.liftF[KBLangK, Seq[Binding]](bindingsOf(p))
  def phosphoSitesF(kinase: Protein, substrate: Protein): FreeK[KBLangK, Seq[Site]] =
    FreeK.liftF[KBLangK, Seq[Site]](phosphoSites(kinase, substrate))

  implicit def functorKAInstance: FunctorKA[KBLangK] =
    new FunctorKA[KBLangK] {
      def transform[K[_], L[_], A](fk: KBLangK[K, A])(f: K ~> L): KBLangK[L, A] = fk match {
        case SitesOf(p) => SitesOf(p)
        case BindingsOf(p) => BindingsOf(p)
        case PhosphoSites(k, s) => PhosphoSites(k, s)
      }
    }
}
