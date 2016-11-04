package proteinrefinery.lib

import nutcracker.util.ContU
import nutcracker.util.ContU._
import nutcracker.{Antichain, DSet, IncSet, Propagation}
import proteinrefinery.util.DomType.AntichainDomType
import proteinrefinery.util.OnceTrigger.Fire
import proteinrefinery.util.{OnceTrigger, Tracking}

import scala.language.higherKinds
import scalaz.{Bind, Monad}
import scalaz.std.list._
import scalaz.syntax.monad._

object Nuggets {

  private object DomTypes {
    implicit object Rules extends AntichainDomType[Rule]
    implicit object PhosphoSites extends AntichainDomType[λ[Ref[_] => PhosphoTarget]]
    implicit object Kinases extends AntichainDomType[ProteinPattern]
  }

}

trait Nuggets[M[_], Ref[_]] {
  import Nuggets._
  import Nuggets.DomTypes._

  def IncSets: nutcracker.IncSets[M, Ref]

  def addNuggets(
    rules: List[Rule[Ref]] = Nil,
    phosphoSites: List[(Protein, Protein, SiteLabel)] = Nil
  )(implicit
    P: Propagation[M, Ref],
    T: Tracking[M, Ref],
    M: Monad[M]
  ): M[Unit] = {
    import scalaz.syntax.traverse._
    val a = rules.traverse_(addRuleF(_))
    val b = phosphoSites.traverse_(kss => addPhosphoTargetF(kss._1, kss._2, kss._3))
    M.apply2(a, b)((_, _) => ())
  }

  // basic programs for adding nuggets
  def addRuleF(r: Rule[Ref])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Bind[M]): M[Unit] =
    P.cell(Antichain(r)) >>= { T.track[λ[A[_] => Antichain[Rule[A]]]](_) }
  def addPhosphoTargetF(kinase: Protein, substrate: Protein, site: SiteLabel)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Bind[M]): M[Unit] =
    P.cell(Antichain(PhosphoTarget(kinase, substrate, site))) >>= { T.track[λ[A[_] => Antichain[PhosphoTarget]]](_) }
  def addKinaseActivityF(activeState: ProteinPattern[Ref])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Bind[M]): M[Unit] =
    P.cell(Antichain(activeState)) >>= { T.track[λ[A[_] => Antichain[ProteinPattern[A]]]](_) }

  // basic programs for querying nuggets
  def rulesF(f: Rule[Ref] => OnceTrigger[Rule.Ref[Ref] => M[Unit]])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): M[Unit] =
    T.thresholdQuery[λ[Var[_] => Antichain[Rule[Var]]]](DomTypes.Rules)(r => f(r.value))
  def phosphoTargetsF(f: PhosphoTarget => OnceTrigger[PhosphoTarget.Ref[Ref] => M[Unit]])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): M[Unit] =
    T.thresholdQuery[λ[Var[_] => Antichain[PhosphoTarget]]](DomTypes.PhosphoSites)(pt => f(pt.value))
  def kinaseActivityF(p: Protein)(f: ProteinPattern.Ref[Ref] => M[Unit])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): M[Unit] =
    T.thresholdQuery[λ[Var[_] => Antichain[ProteinPattern[Var]]]](DomTypes.Kinases)(pp =>
      if(pp.value.protein == p) OnceTrigger.Fire(f)
      else OnceTrigger.Discard()
    )

  // queries in CPS style
  def rulesC(p: Rule[Ref] => OnceTrigger[Unit])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): ContU[M, Rule.Ref[Ref]] =
    ContU(f => rulesF(p andThen (_.map(_ => f))))
  def phosphoTargetsC(p: PhosphoTarget => OnceTrigger[Unit])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): ContU[M, PhosphoTarget.Ref[Ref]] =
    ContU(f => phosphoTargetsF(p andThen (_.map(_ => f))))
  def kinaseActivityC(kinase: Protein)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): ContU[M, ProteinPattern.Ref[Ref]] =
    ContU(f => kinaseActivityF(kinase)(f))

  // derived queries

  def bindingsOfF(p: Protein)(f: Binding.Ref[Ref] => M[Unit])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Monad[M]): M[Unit] =
    rulesF(_ => OnceTrigger.Fire(ruleRef => Rule.linksAgentToC(ruleRef)(p).apply(f)))
  def bindingsOfC(p: Protein)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Monad[M]): ContU[M, Binding.Ref[Ref]] =
    ContU(f => bindingsOfF(p)(f))
  def bindingsOfS(p: Protein)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Monad[M]): M[Ref[DSet[Ref, Antichain[Binding[Ref]]]]] =
    DSet.collect(bindingsOfC(p))

  def phosphoSitesF(kinase: Protein, substrate: Protein)(f: SiteLabel => M[Unit])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): M[Unit] =
    phosphoTargetsF(pt => {
      val PhosphoTarget(k, s, ss) = pt
      if(kinase == k && substrate == s) OnceTrigger.Fire(_ => f(ss))
      else OnceTrigger.Discard()
    })
  def phosphoSitesC(kinase: Protein, substrate: Protein)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): ContU[M, SiteLabel] = // TODO: return Site.Ref
    ContU(f => phosphoSitesF(kinase, substrate)(f))
  def phosphoSitesS(kinase: Protein, substrate: Protein)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Bind[M]): M[Ref[IncSet[SiteLabel]]] =
    IncSets.collect(phosphoSitesC(kinase, substrate))

  def phosphoSitesS(substrate: Protein)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Bind[M]): M[Ref[IncSet[SiteLabel]]] =
    for {
      res <- IncSets.init[SiteLabel]
      _   <- phosphoTargetsF(pt => {
        val PhosphoTarget(k, s, ss) = pt
        if(s == substrate) OnceTrigger.Fire(_ => IncSets.insert(ss, res))
        else OnceTrigger.Discard()
      })
    } yield res
  def phosphoSitesC(substrate: Protein)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Monad[M]): ContU[M, SiteLabel] = // TODO: return Site.Ref
    phosphoSitesS(substrate).map(IncSets.forEach(_)).wrapEffect

  def kinasesOfF(substrate: Protein, site: SiteLabel)(f: Protein => M[Unit])(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): M[Unit] =
    phosphoTargetsF(pt => {
      val PhosphoTarget(k, s, ss) = pt
      if(substrate == s && site == ss) OnceTrigger.Fire(_ => f(k))
      else OnceTrigger.Discard()
    })
  def kinasesOfC(substrate: Protein, site: SiteLabel)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): ContU[M, Protein] = // TODO: return Protein.Ref
    ContU(f => kinasesOfF(substrate, site)(f))
  def kinasesOfS(substrate: Protein, site: SiteLabel)(implicit P: Propagation[M, Ref], T: Tracking[M, Ref], M: Bind[M]): M[Ref[IncSet[Protein]]] =
    IncSets.collect(kinasesOfC(substrate, site))

  def forEachRule(implicit P: Propagation[M, Ref], T: Tracking[M, Ref]): ContU[M, Rule.Ref[Ref]] =
    rulesC(r => Fire(()))
}