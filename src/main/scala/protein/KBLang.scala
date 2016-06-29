package protein

import nutcracker.{IncSet, PropagationLang}
import nutcracker.IncSet.IncSetRef

import scala.language.higherKinds
import nutcracker.util.{ContF, FreeK, FunctorKA, InjectK, Lst}
import protein.capability.Rule
import protein.mechanism.{Binding, Protein, Site}

import scalaz.~>

sealed trait KBLang[K[_], A]

object KBLang {

  // Definition of KB instruction set
  case class AddRule[K[_]](r: Rule) extends KBLang[K, Unit]
  case class AddPhosphoSite[K[_]](kinase: Protein, substrate: Protein, site: Site) extends KBLang[K, Unit]
  case class Rules[K[_]](f: Rule => Lst[K[Unit]]) extends KBLang[K, Unit]
  case class PhosphoSites[K[_]](kinase: Protein, substrate: Protein, f: Site => K[Unit]) extends KBLang[K, Unit]

  // smart constructors for KB instructions
  def addRule[K[_]](r: Rule): KBLang[K, Unit] = AddRule(r)
  def addPhosphoSite[K[_]](kinase: Protein, substrate: Protein, site: Site): KBLang[K, Unit] = AddPhosphoSite(kinase, substrate, site)
  def rules[K[_]](f: Rule => Lst[K[Unit]]): KBLang[K, Unit] = Rules(f)
  def phosphoSites[K[_]](kinase: Protein, substrate: Protein)(f: Site => K[Unit]): KBLang[K, Unit] = PhosphoSites(kinase, substrate, f)

  // KB instructions lifted to Free programs
  def addRuleF[F[_[_], _]](r: Rule)(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](addRule(r))
  def addPhosphoSiteF[F[_[_], _]](kinase: Protein, substrate: Protein, site: Site)(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](addPhosphoSite(kinase, substrate, site))
  def rulesF[F[_[_], _]](f: Rule => Lst[FreeK[F, Unit]])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](rules(f))
  def phosphoSitesF[F[_[_], _]](kinase: Protein, substrate: Protein)(f: Site => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](phosphoSites[FreeK[F, ?]](kinase, substrate)(f))

  // derived programs
  def bindingsOfF[F[_[_], _]](p: Protein)(f: Binding => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    rulesF[F](rule => (rule.linksAgentTo(p), f) mapRev_::: Lst.empty)

  // KB queries in CPS style
  def bindingsOfC[F[_[_], _]](p: Protein)(implicit inj: InjectK[KBLang, F]): ContF[F, Binding] =
    ContF(f => bindingsOfF[F](p)(f))
  def rulesC[F[_[_], _]](implicit inj: InjectK[KBLang, F]): ContF[F, Rule] =
    ContF(f => rulesF[F](f andThen Lst.singleton))
  def phosphoSitesC[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit inj: InjectK[KBLang, F]): ContF[F, Site] =
    ContF(f => phosphoSitesF[F](kinase, substrate)(f))

  // KB queries returning a result set
  def bindingsOfS[F[_[_], _]](p: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Binding]] =
    IncSet.collect(bindingsOfC(p))
  def phosphoSitesS[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Site]] =
    IncSet.collect(phosphoSitesC(kinase, substrate))

  implicit def functorKAInstance: FunctorKA[KBLang] =
    new FunctorKA[KBLang] {
      def transform[K[_], L[_], A](fk: KBLang[K, A])(kl: K ~> L): KBLang[L, A] = fk match {
        case AddRule(r) => AddRule(r)
        case AddPhosphoSite(kin, sub, s) => AddPhosphoSite(kin, sub, s)
        case Rules(f) => Rules(r => f(r).map(kl(_)))
        case PhosphoSites(k, s, f) => PhosphoSites(k, s, x => kl(f(x)))
      }
    }
}
