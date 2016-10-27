package proteinrefinery.lib

import nutcracker.DSet.DSetRef
import nutcracker.IncSet._
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
    implicit object PhosphoSites extends AntichainDomType[PhosphoTarget]
    implicit object Kinases extends AntichainDomType[ProteinPattern]
  }

}

trait Nuggets[M[_]] {
  import Nuggets._
  import Nuggets.DomTypes._

  def addNuggets(
    rules: List[Rule] = Nil,
    phosphoSites: List[(Protein, Protein, SiteLabel)] = Nil
  )(implicit
    P: Propagation[M],
    T: Tracking[M],
    M: Monad[M]
  ): M[Unit] = {
    import scalaz.syntax.traverse._
    val a = rules.traverse_(addRuleF(_))
    val b = phosphoSites.traverse_(kss => addPhosphoTargetF(kss._1, kss._2, kss._3))
    M.apply2(a, b)((_, _) => ())
  }

  // basic programs for adding nuggets
  def addRuleF(r: Rule)(implicit P: Propagation[M], T: Tracking[M], M: Bind[M]): M[Unit] =
    P.cell(Antichain(r)) >>= { T.track(_) }
  def addPhosphoTargetF(kinase: Protein, substrate: Protein, site: SiteLabel)(implicit P: Propagation[M], T: Tracking[M], M: Bind[M]): M[Unit] =
    P.cell(Antichain(PhosphoTarget(kinase, substrate, site))) >>= { T.track(_) }
  def addKinaseActivityF(activeState: ProteinPattern)(implicit P: Propagation[M], T: Tracking[M], M: Bind[M]): M[Unit] =
    P.cell(Antichain(activeState)) >>= { T.track(_) }

  // basic programs for querying nuggets
  def rulesF(f: Rule => OnceTrigger[Rule.Ref => M[Unit]])(implicit P: Propagation[M], T: Tracking[M]): M[Unit] =
    T.thresholdQuery(DomTypes.Rules)(r => f(r.value))
  def phosphoTargetsF(f: PhosphoTarget => OnceTrigger[PhosphoTarget.Ref => M[Unit]])(implicit P: Propagation[M], T: Tracking[M]): M[Unit] =
    T.thresholdQuery(DomTypes.PhosphoSites)(pt => f(pt.value))
  def kinaseActivityF(p: Protein)(f: ProteinPattern.Ref => M[Unit])(implicit P: Propagation[M], T: Tracking[M]): M[Unit] =
    T.thresholdQuery(DomTypes.Kinases)(pp =>
      if(pp.value.protein == p) OnceTrigger.Fire(f)
      else OnceTrigger.Discard()
    )

  // queries in CPS style
  def rulesC(p: Rule => OnceTrigger[Unit])(implicit P: Propagation[M], T: Tracking[M]): ContU[M, Rule.Ref] =
    ContU(f => rulesF(p andThen (_.map(_ => f))))
  def phosphoTargetsC(p: PhosphoTarget => OnceTrigger[Unit])(implicit P: Propagation[M], T: Tracking[M]): ContU[M, PhosphoTarget.Ref] =
    ContU(f => phosphoTargetsF(p andThen (_.map(_ => f))))
  def kinaseActivityC(kinase: Protein)(implicit P: Propagation[M], T: Tracking[M]): ContU[M, ProteinPattern.Ref] =
    ContU(f => kinaseActivityF(kinase)(f))

  // derived queries

  def bindingsOfF(p: Protein)(f: Binding.Ref => M[Unit])(implicit P: Propagation[M], T: Tracking[M], M: Monad[M]): M[Unit] =
    rulesF(_ => OnceTrigger.Fire(ruleRef => Rule.linksAgentToC(ruleRef)(p).apply(f)))
  def bindingsOfC(p: Protein)(implicit P: Propagation[M], T: Tracking[M], M: Monad[M]): ContU[M, Binding.Ref] =
    ContU(f => bindingsOfF(p)(f))
  def bindingsOfS(p: Protein)(implicit P: Propagation[M], T: Tracking[M], M: Monad[M]): M[DSetRef[Antichain[Binding]]] =
    DSet.collect(bindingsOfC(p))

  def phosphoSitesF(kinase: Protein, substrate: Protein)(f: SiteLabel => M[Unit])(implicit P: Propagation[M], T: Tracking[M]): M[Unit] =
    phosphoTargetsF(pt => {
      val PhosphoTarget(k, s, ss) = pt
      if(kinase == k && substrate == s) OnceTrigger.Fire(_ => f(ss))
      else OnceTrigger.Discard()
    })
  def phosphoSitesC(kinase: Protein, substrate: Protein)(implicit P: Propagation[M], T: Tracking[M]): ContU[M, SiteLabel] = // TODO: return Site.Ref
    ContU(f => phosphoSitesF(kinase, substrate)(f))
  def phosphoSitesS(kinase: Protein, substrate: Protein)(implicit P: Propagation[M], T: Tracking[M], M: Bind[M]): M[IncSetRef[SiteLabel]] =
    IncSet.collect(phosphoSitesC(kinase, substrate))

  def phosphoSitesS(substrate: Protein)(implicit P: Propagation[M], T: Tracking[M], M: Bind[M]): M[IncSetRef[SiteLabel]] =
    for {
      res <- IncSet.init[M, SiteLabel]
      _   <- phosphoTargetsF(pt => {
        val PhosphoTarget(k, s, ss) = pt
        if(s == substrate) OnceTrigger.Fire(_ => IncSet.insert(ss, res))
        else OnceTrigger.Discard()
      })
    } yield res
  def phosphoSitesC(substrate: Protein)(implicit P: Propagation[M], T: Tracking[M], M: Monad[M]): ContU[M, SiteLabel] = // TODO: return Site.Ref
    phosphoSitesS(substrate).map(IncSet.forEach(_)).wrapEffect

  def kinasesOfF(substrate: Protein, site: SiteLabel)(f: Protein => M[Unit])(implicit P: Propagation[M], T: Tracking[M]): M[Unit] =
    phosphoTargetsF(pt => {
      val PhosphoTarget(k, s, ss) = pt
      if(substrate == s && site == ss) OnceTrigger.Fire(_ => f(k))
      else OnceTrigger.Discard()
    })
  def kinasesOfC(substrate: Protein, site: SiteLabel)(implicit P: Propagation[M], T: Tracking[M]): ContU[M, Protein] = // TODO: return Protein.Ref
    ContU(f => kinasesOfF(substrate, site)(f))
  def kinasesOfS(substrate: Protein, site: SiteLabel)(implicit P: Propagation[M], T: Tracking[M], M: Bind[M]): M[IncSetRef[Protein]] =
    IncSet.collect(kinasesOfC(substrate, site))

  def forEachRule(implicit P: Propagation[M], T: Tracking[M]): ContU[M, Rule.Ref] =
    rulesC(r => Fire(()))
}