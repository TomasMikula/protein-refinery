package protein

import nutcracker.{IncSet, PropagationLang}
import nutcracker.IncSet.IncSetRef

import scala.language.higherKinds
import nutcracker.util.{ContF, FreeK, FunctorKA, InjectK, Lst}
import protein.capability.{ProteinPattern, Rule}
import protein.mechanism.{Binding, Protein, Site}

import scalaz.~>

sealed trait KBLang[K[_], A]

object KBLang {

  // Definition of KB instruction set
  case class AddRule[K[_]](r: Rule) extends KBLang[K, Unit]
  case class AddPhosphoTarget[K[_]](kinase: Protein, substrate: Protein, site: Site) extends KBLang[K, Unit]
  case class AddKinaseActivity[K[_]](activeState: ProteinPattern) extends KBLang[K, Unit]
  case class Rules[K[_]](f: Rule => Lst[K[Unit]]) extends KBLang[K, Unit]
  case class PhosphoTargets[K[_]](f: (Protein, Protein, Site) => K[Unit]) extends KBLang[K, Unit]
  case class KinaseActivity[K[_]](p: Protein, f: ProteinPattern => K[Unit]) extends KBLang[K, Unit]

  // smart constructors for KB instructions
  def addRule[K[_]](r: Rule): KBLang[K, Unit] = AddRule(r)
  def addPhosphoTarget[K[_]](kinase: Protein, substrate: Protein, site: Site): KBLang[K, Unit] = AddPhosphoTarget(kinase, substrate, site)
  def addKinaseActivity[K[_]](activeState: ProteinPattern): KBLang[K, Unit] = AddKinaseActivity(activeState)
  def rules[K[_]](f: Rule => Lst[K[Unit]]): KBLang[K, Unit] = Rules(f)
  def phosphoTargets[K[_]](f: (Protein, Protein, Site) => K[Unit]): KBLang[K, Unit] = PhosphoTargets(f)
  def kinaseActivity[K[_]](p: Protein, f: ProteinPattern => K[Unit]): KBLang[K, Unit] = KinaseActivity(p, f)

  // KB instructions lifted to Free programs
  def addRuleF[F[_[_], _]](r: Rule)(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](addRule(r))
  def addPhosphoTargetF[F[_[_], _]](kinase: Protein, substrate: Protein, site: Site)(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](addPhosphoTarget(kinase, substrate, site))
  def addKinaseActivityF[F[_[_], _]](activeState: ProteinPattern)(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](addKinaseActivity(activeState))
  def rulesF[F[_[_], _]](f: Rule => Lst[FreeK[F, Unit]])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](rules(f))
  def phosphoTargetsF[F[_[_], _]](f: (Protein, Protein, Site) => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](phosphoTargets(f))
  def kinaseActivityF[F[_[_], _]](p: Protein)(f: ProteinPattern => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[KBLang, F, Unit](kinaseActivity(p, f))

  // KB queries in CPS style
  def rulesC[F[_[_], _]](implicit inj: InjectK[KBLang, F]): ContF[F, Rule] =
    ContF(f => rulesF[F](f andThen Lst.singleton))
  def phosphoTargetsC[F[_[_], _]](implicit inj: InjectK[KBLang, F]): ContF[F, (Protein, Protein, Site)] =
    ContF(f => phosphoTargetsF[F]((k, s, ss) => f((k, s, ss))))
  def kinaseActivityC[F[_[_], _]](kinase: Protein)(implicit inj: InjectK[KBLang, F]): ContF[F, ProteinPattern] =
    ContF(f => kinaseActivityF[F](kinase)(f))

  // derived queries

  def bindingsOfF[F[_[_], _]](p: Protein)(f: Binding => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    rulesF[F](rule => (rule.linksAgentTo(p), f) mapRev_::: Lst.empty)
  def bindingsOfC[F[_[_], _]](p: Protein)(implicit inj: InjectK[KBLang, F]): ContF[F, Binding] =
    ContF(f => bindingsOfF[F](p)(f))
  def bindingsOfS[F[_[_], _]](p: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Binding]] =
    IncSet.collect(bindingsOfC(p))

  def phosphoSitesF[F[_[_], _]](kinase: Protein, substrate: Protein)(f: Site => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    phosphoTargetsF[F]((k, s, ss) => if(kinase == k && substrate == s) f(ss) else FreeK.pure(()))
  def phosphoSitesC[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit inj: InjectK[KBLang, F]): ContF[F, Site] =
    ContF(f => phosphoSitesF[F](kinase, substrate)(f))
  def phosphoSitesS[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Site]] =
    IncSet.collect(phosphoSitesC(kinase, substrate))

  def phosphoSitesS[F[_[_], _]](substrate: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Site]] =
    for {
      res <- IncSet.initF[F, Site]
      _   <- phosphoTargetsF[F]((k, s, ss) => if(s == substrate) IncSet.insert(ss, res) else FreeK.pure(()))
    } yield res
  def phosphoSitesC[F[_[_], _]](substrate: Protein)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): ContF[F, Site] =
    ContF.wrapEffect(phosphoSitesS[F](substrate).map(IncSet.forEach(_)))

  def kinasesOfF[F[_[_], _]](substrate: Protein, site: Site)(f: Protein => FreeK[F, Unit])(implicit inj: InjectK[KBLang, F]): FreeK[F, Unit] =
    phosphoTargetsF[F]((k, s, ss) => if(substrate == s && site == ss) f(k) else FreeK.pure(()))
  def kinasesOfC[F[_[_], _]](substrate: Protein, site: Site)(implicit inj: InjectK[KBLang, F]): ContF[F, Protein] =
    ContF(f => kinasesOfF[F](substrate, site)(f))
  def kinasesOfS[F[_[_], _]](substrate: Protein, site: Site)(implicit i: InjectK[KBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Protein]] =
    IncSet.collect(kinasesOfC(substrate, site))


  implicit def functorKAInstance: FunctorKA[KBLang] =
    new FunctorKA[KBLang] {
      def transform[K[_], L[_], A](fk: KBLang[K, A])(kl: K ~> L): KBLang[L, A] = fk match {
        case AddRule(r) => AddRule(r)
        case AddPhosphoTarget(kin, sub, s) => AddPhosphoTarget(kin, sub, s)
        case AddKinaseActivity(pp) => AddKinaseActivity(pp)
        case Rules(f) => Rules(r => f(r).map(kl(_)))
        case PhosphoTargets(f) => PhosphoTargets((k, s, ss) => kl(f(k, s, ss)))
        case KinaseActivity(p, f) => KinaseActivity(p, pp => kl(f(pp)))
      }
    }
}
