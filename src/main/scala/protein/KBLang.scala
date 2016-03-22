package protein

import scala.language.higherKinds

import nutcracker.util.FreeK
import protein.mechanism.{Site, Protein}

sealed trait KBLang[A]

object KBLang {
  type KBLangK[K[_], A] = KBLang[A]

  case class SitesOf(p: Protein) extends KBLang[Seq[Site]]
  case class BindingsOf(p: Protein) extends KBLang[Seq[capability.Binding]]
  case class PhosphoSites(kinase: Protein, substrate: Protein) extends KBLang[Seq[Site]]

  def sitesOf[K[_]](p: Protein): KBLangK[K, Seq[Site]] = SitesOf(p)
  def bindingsOf[K[_]](p: Protein): KBLangK[K, Seq[capability.Binding]] = BindingsOf(p)
  def phosphoSites[K[_]](kinase: Protein, substrate: Protein): KBLangK[K, Seq[Site]] = PhosphoSites(kinase, substrate)

  def sitesOfF(p: Protein): FreeK[KBLangK, Seq[Site]] =
    FreeK.suspend[KBLangK, Seq[Site]](sitesOf(p))
  def bindingsOfF(p: Protein): FreeK[KBLangK, Seq[capability.Binding]] =
    FreeK.suspend[KBLangK, Seq[capability.Binding]](bindingsOf(p))
  def phosphoSitesF(kinase: Protein, substrate: Protein): FreeK[KBLangK, Seq[Site]] =
    FreeK.suspend[KBLangK, Seq[Site]](phosphoSites(kinase, substrate))
}
