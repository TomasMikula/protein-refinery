package protein.lib

import nutcracker.DSet.DSetRef
import nutcracker.IncSet._
import nutcracker.PropagationLang._
import nutcracker.util.{ContF, FreeK, InjectK}
import nutcracker.util.ContF._
import nutcracker.{DSet, IncSet, PropagationLang}
import protein.util.TrackLang._
import protein.util.{Antichain, DomType, OnceTrigger, TrackLang}

import scala.language.higherKinds

object Nuggets {
  type PhosphoTarget = (Protein, Protein, Site)

  type PhosphoTragetRef = Antichain.Ref[PhosphoTarget]
  type ProteinPatternRef = Antichain.Ref[ProteinPattern]

  private object DomTypes {
    implicit object Rules extends Antichain.DomType[Antichain[Rule]]
    implicit object PhosphoSites extends Antichain.DomType[Antichain[PhosphoTarget]]
    implicit object Kinases extends Antichain.DomType[Antichain[ProteinPattern]]
  }

  import DomTypes._

  def addAll[F[_[_], _]](
    rules: List[Rule] = Nil,
    phosphoSites: List[(Protein, Protein, Site)] = Nil
  )(implicit
    i: InjectK[PropagationLang, F],
    j: InjectK[TrackLang, F]
  ): FreeK[F, Unit] = {
    val a = rules.map(addRuleF[F](_))
    val b = phosphoSites.map(kss => addPhosphoTargetF[F](kss._1, kss._2, kss._3))
    FreeK.sequence_(FreeK.sequence_(a), FreeK.sequence_(b))
  }

  // basic programs for adding nuggets
  def addRuleF[F[_[_], _]](r: Rule)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    cellF(Antichain(r)).inject[F] >>= { trackF(_) }
  def addPhosphoTargetF[F[_[_], _]](kinase: Protein, substrate: Protein, site: Site)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    cellF(Antichain((kinase, substrate, site))).inject[F] >>= { trackF(_) }
  def addKinaseActivityF[F[_[_], _]](activeState: ProteinPattern)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    cellF(Antichain(activeState)).inject[F] >>= { trackF(_) }

  // basic programs for querying nuggets
  def rulesF[F[_[_], _]](f: Rule => OnceTrigger[Rule.Ref => FreeK[F, Unit]])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    thresholdQuery(DomTypes.Rules: DomType.Aux[Antichain[Rule], Nothing, Nothing])(r => f(r.value))
  def phosphoTargetsF[F[_[_], _]](f: PhosphoTarget => OnceTrigger[PhosphoTragetRef => FreeK[F, Unit]])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    thresholdQuery(DomTypes.PhosphoSites)(pt => f(pt.value))
  def kinaseActivityF[F[_[_], _]](p: Protein)(f: ProteinPatternRef => FreeK[F, Unit])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    thresholdQuery(DomTypes.Kinases)(pp =>
      if(pp.value.protein == p) OnceTrigger.Fire(f)
      else OnceTrigger.Discard()
    )

  // queries in CPS style
  def rulesC[F[_[_], _]](p: Rule => OnceTrigger[Unit])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): ContF[F, Rule.Ref] =
    ContF(f => rulesF[F](p andThen (_.map(_ => f))))
  def phosphoTargetsC[F[_[_], _]](p: PhosphoTarget => OnceTrigger[Unit])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): ContF[F, PhosphoTragetRef] =
    ContF(f => phosphoTargetsF[F](p andThen (_.map(_ => f))))
  def kinaseActivityC[F[_[_], _]](kinase: Protein)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): ContF[F, ProteinPatternRef] =
    ContF(f => kinaseActivityF[F](kinase)(f))

  // derived queries

  def bindingsOfF[F[_[_], _]](p: Protein)(f: Binding.Ref => FreeK[F, Unit])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    rulesF[F](_ => OnceTrigger.Fire(ruleRef => Rule.linksAgentToC(ruleRef)(p).apply(f)))
  def bindingsOfC[F[_[_], _]](p: Protein)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): ContF[F, Binding.Ref] =
    ContF(f => bindingsOfF[F](p)(f))
  def bindingsOfS[F[_[_], _]](p: Protein)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, DSetRef[Antichain[Binding]]] =
    DSet.collect(bindingsOfC[F](p))

  def phosphoSitesF[F[_[_], _]](kinase: Protein, substrate: Protein)(f: Site => FreeK[F, Unit])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    phosphoTargetsF[F](kss => {
      val (k, s, ss) = kss
      if(kinase == k && substrate == s) OnceTrigger.Fire(_ => f(ss))
      else OnceTrigger.Discard()
    })
  def phosphoSitesC[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): ContF[F, Site] =
    ContF(f => phosphoSitesF[F](kinase, substrate)(f))
  def phosphoSitesS[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, IncSetRef[Site]] =
    IncSet.collect(phosphoSitesC(kinase, substrate))

  def phosphoSitesS[F[_[_], _]](substrate: Protein)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, IncSetRef[Site]] =
    for {
      res <- IncSet.init[F, Site]
      _   <- phosphoTargetsF[F](kss => {
        val (k, s, ss) = kss
        if(s == substrate) OnceTrigger.Fire(_ => IncSet.insert(ss, res))
        else OnceTrigger.Discard()
      })
    } yield res
  def phosphoSitesC[F[_[_], _]](substrate: Protein)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): ContF[F, Site] =
    phosphoSitesS[F](substrate).map(IncSet.forEach(_)).wrapEffect

  def kinasesOfF[F[_[_], _]](substrate: Protein, site: Site)(f: Protein => FreeK[F, Unit])(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, Unit] =
    phosphoTargetsF[F](kss => {
      val (k, s, ss) = kss
      if(substrate == s && site == ss) OnceTrigger.Fire(_ => f(k))
      else OnceTrigger.Discard()
    })
  def kinasesOfC[F[_[_], _]](substrate: Protein, site: Site)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): ContF[F, Protein] =
    ContF(f => kinasesOfF[F](substrate, site)(f))
  def kinasesOfS[F[_[_], _]](substrate: Protein, site: Site)(implicit i: InjectK[PropagationLang, F], j: InjectK[TrackLang, F]): FreeK[F, IncSetRef[Protein]] =
    IncSet.collect(kinasesOfC(substrate, site))

}