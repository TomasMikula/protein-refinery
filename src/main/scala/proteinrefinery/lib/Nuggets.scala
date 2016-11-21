package proteinrefinery.lib

import nutcracker.util.{ContU, EqualK}
import nutcracker.util.ContU._
import nutcracker.{Antichain, DSet, IncSet, Propagation}
import proteinrefinery.util.DomType.AntichainDomType
import proteinrefinery.util.OnceTrigger.Fire
import proteinrefinery.util.{OnceTrigger, Tracking}

import scala.language.higherKinds
import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.equal._
import scalaz.syntax.monad._

object Nuggets {

  private object DomTypes {
    implicit object Rules extends AntichainDomType[Rule]
    implicit object Kinases extends AntichainDomType[ProteinPattern]
  }

}

trait Nuggets[M[_], Ref[_]] {
  import Nuggets._
  import Nuggets.DomTypes._

  implicit val M: Monad[M]
  implicit val RefEquality: EqualK[Ref]
  implicit val Propagation: Propagation[M, Ref]
  implicit val Tracking: Tracking[M, Ref]

  def IncSets: nutcracker.IncSets[M, Ref]
  def PhosphoTargetOps: PhosphoTarget.Ops[M, Ref]

  import Propagation._
  import Tracking._

  def addNuggets(
    rules: List[Rule[Ref]] = Nil,
    phosphoSites: List[(Protein, Protein, SiteLabel)] = Nil
  ): M[Unit] = {
    import scalaz.syntax.traverse._
    val a = rules.traverse_(addRuleF(_) map (_ => ()))
    val b = phosphoSites.traverse_(kss => addPhosphoTargetF(kss._1, kss._2, kss._3))
    M.apply2(a, b)((_, _) => ())
  }

  // basic programs for adding nuggets
  def addRuleF(r: Rule[Ref]): M[Rule.Ref[Ref]] =
    cell(Antichain(r)) >>! { track[λ[A[_] => Antichain[Rule[A]]]](_) }
  def addKinaseActivityF(activeState: ProteinPattern[Ref]): M[Unit] =
    cell(Antichain(activeState)) >>= { track[λ[A[_] => Antichain[ProteinPattern[A]]]](_) }

  // derived programs for adding nuggets
  def addPhosphoTargetF(kinase: Protein, substrate: Protein, site: SiteLabel): M[Unit] =
    for {
      pt <- PhosphoTargetOps.define(kinase, substrate, ISite(site))
      _ <- addRuleF(pt.witness)
    } yield ()

  // basic programs for querying nuggets
  def rulesF(f: Rule[Ref] => OnceTrigger[Rule.Ref[Ref] => M[Unit]]): M[Unit] =
    thresholdQuery[λ[Var[_] => Antichain[Rule[Var]]]](DomTypes.Rules)(r => f(r.value))
  def kinaseActivityF(p: Protein)(f: ProteinPattern.Ref[Ref] => M[Unit]): M[Unit] =
    thresholdQuery[λ[Var[_] => Antichain[ProteinPattern[Var]]]](DomTypes.Kinases)(pp =>
      if(pp.value.protein === p) OnceTrigger.Fire(f)
      else OnceTrigger.Discard()
    )

  // queries in CPS style
  def rulesC(p: Rule[Ref] => OnceTrigger[Unit]): ContU[M, Rule.Ref[Ref]] =
    ContU(f => rulesF(p andThen (_.map(_ => f))))
  def kinaseActivityC(kinase: Protein): ContU[M, ProteinPattern.Ref[Ref]] =
    ContU(f => kinaseActivityF(kinase)(f))

  // derived queries

  def bindingsOfF(p: Protein)(f: Binding.Ref[Ref] => M[Unit]): M[Unit] =
    rulesF(_ => OnceTrigger.Fire(ruleRef => Rule.linksAgentToC(ruleRef)(p).apply(f)))
  def bindingsOfC(p: Protein): ContU[M, Binding.Ref[Ref]] =
    ContU(f => bindingsOfF(p)(f))
  def bindingsOfS(p: Protein): M[Ref[DSet[Ref, Antichain[Binding[Ref]]]]] =
    DSet.collect(bindingsOfC(p))

  def phosphoTargetsF(f: PhosphoTarget.Ref[Ref] => M[Unit]): M[Unit] =
    rulesF(_ => OnceTrigger.Fire(ruleRef => Rule.phosphorylationsC(ruleRef).apply(f)))

  def phosphoSitesF(kinase: Protein, substrate: Protein)(f: ISite[Ref] => M[Unit]): M[Unit] =
    phosphoTargetsF(ptr => observe(ptr).by(apt => {
      val pt = apt.value
      if(kinase === pt.kinase && substrate === pt.substrate) {
        val now = f(pt.targetSite)
        (Some(now), Some((d, δ) => ???))
      }
      else (None, Some((d, δ) => ???))
    }))
  def phosphoSitesC(kinase: Protein, substrate: Protein): ContU[M, ISite[Ref]] = // TODO: return Site.Ref
    ContU(f => phosphoSitesF(kinase, substrate)(f))
  def phosphoSitesS(kinase: Protein, substrate: Protein): M[Ref[IncSet[ISite[Ref]]]] =
    IncSets.collect(phosphoSitesC(kinase, substrate))

  def phosphoSitesS(substrate: Protein): M[Ref[IncSet[ISite[Ref]]]] =
    for {
      res <- IncSets.init[ISite[Ref]]
      _   <- phosphoTargetsF(ptr => observe(ptr).by(apt => {
        val pt = apt.value
        if(pt.substrate === substrate) (Some(IncSets.insert(pt.targetSite, res)), Some((d, δ) => ???))
        else (None, Some((d, δ) => ???))
      }))
    } yield res
  def phosphoSitesC(substrate: Protein): ContU[M, ISite[Ref]] = // TODO: return Site.Ref
    phosphoSitesS(substrate).map(IncSets.forEach(_)).wrapEffect

  def kinasesOfF(substrate: Protein, site: SiteLabel)(f: Protein => M[Unit]): M[Unit] =
    phosphoTargetsF(ptr => observe(ptr).by(apt => {
      val pt = apt.value
      if(substrate === pt.substrate && ISite[Ref](site) === pt.targetSite) (Some(f(pt.kinase)), Some((d, δ) => ???))
      else (None, Some((d, δ) => ???))
    }))
  def kinasesOfC(substrate: Protein, site: SiteLabel): ContU[M, Protein] = // TODO: return Protein.Ref
    ContU(f => kinasesOfF(substrate, site)(f))
  def kinasesOfS(substrate: Protein, site: SiteLabel): M[Ref[IncSet[Protein]]] =
    IncSets.collect(kinasesOfC(substrate, site))

  def forEachRule: ContU[M, Rule.Ref[Ref]] =
    rulesC(r => Fire(()))
}