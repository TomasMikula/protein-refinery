package protein

import nutcracker.{IncSet, PropagationLang}
import nutcracker.IncSet.IncSetRef

import scala.language.higherKinds
import nutcracker.util.{ContF, FreeK, FunctorKA, InjectK}
import protein.capability.Rule
import protein.mechanism.{Binding, Protein, Site}

import scalaz.~>

sealed trait KBLang[K[_], A]

object KBLang {

  // Definition of KB instruction set
  case class AddRule[K[_]](r: Rule) extends KBLang[K, Unit]
  case class AddPhosphoSite[K[_]](kinase: Protein, substrate: Protein, site: Site) extends KBLang[K, Unit]
  case class BindingsOf[K[_]](p: Protein, f: Binding => K[Unit]) extends KBLang[K, Unit]
  case class PhosphoSites[K[_]](kinase: Protein, substrate: Protein, f: Site => K[Unit]) extends KBLang[K, Unit]

  // smart constructors for KB instructions
  def addRule[K[_]](r: Rule): KBLang[K, Unit] = AddRule(r)
  def addPhosphoSite[K[_]](kinase: Protein, substrate: Protein, site: Site): KBLang[K, Unit] = AddPhosphoSite(kinase, substrate, site)
  def bindingsOf[K[_]](p: Protein)(f: Binding => K[Unit]): KBLang[K, Unit] = BindingsOf(p, f)
  def phosphoSites[K[_]](kinase: Protein, substrate: Protein)(f: Site => K[Unit]): KBLang[K, Unit] = PhosphoSites(kinase, substrate, f)

  // KB instructions lifted to Free programs
  def addRuleF[F[_[_], _]](r: Rule)(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](addRule(r))
  def addPhosphoSiteF[F[_[_], _]](kinase: Protein, substrate: Protein, site: Site)(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](addPhosphoSite(kinase, substrate, site))
  def bindingsOfF[F[_[_], _]](p: Protein)(f: Binding => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](bindingsOf[FreeK[F, ?]](p)(f))
  def phosphoSitesF[F[_[_], _]](kinase: Protein, substrate: Protein)(f: Site => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](phosphoSites[FreeK[F, ?]](kinase, substrate)(f))

  // KB query instructions in CPS style
  def bindingsOfC[F[_[_], _]](p: Protein)(implicit inj: InjectK[KBLang, F]): ContF[F, Binding] =
    ContF(f => bindingsOfF[F](p)(f))
  def phosphoSitesC[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit inj: InjectK[KBLang, F]): ContF[F, Site] =
    ContF(f => phosphoSitesF[F](kinase, substrate)(f))

  // KB query instructions returning a result set
  def bindingsOfS[F[_[_], _]](p: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Binding]] =
    IncSet.collect(bindingsOfC(p))
  def phosphoSitesS[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Site]] =
    IncSet.collect(phosphoSitesC(kinase, substrate))

  implicit def functorKAInstance: FunctorKA[KBLang] =
    new FunctorKA[KBLang] {
      def transform[K[_], L[_], A](fk: KBLang[K, A])(kl: K ~> L): KBLang[L, A] = fk match {
        case AddRule(r) => AddRule(r)
        case AddPhosphoSite(kin, sub, s) => AddPhosphoSite(kin, sub, s)
        case BindingsOf(p, f) => BindingsOf(p, b => kl(f(b)))
        case PhosphoSites(k, s, f) => PhosphoSites(k, s, x => kl(f(x)))
      }
    }
}
