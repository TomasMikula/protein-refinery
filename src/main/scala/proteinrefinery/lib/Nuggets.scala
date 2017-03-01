package proteinrefinery.lib

import nutcracker.util.{ContU, EqualK}
import nutcracker.util.ContU._
import nutcracker.{Discrete, IncSet, Propagation, Trigger}
import proteinrefinery.util.DomType.DiscreteDomType
import proteinrefinery.util.OnceTrigger.Fire
import proteinrefinery.util.{OnceTrigger, Tracking}

import scala.language.higherKinds
import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.equal._
import scalaz.syntax.monad._

object Nuggets {

  private object DomTypes {
    implicit object Rules extends DiscreteDomType[Rule]
    implicit object Kinases extends DiscreteDomType[ProteinPattern]
  }

}

trait Nuggets[M[_], Ref[_]] {
  import Nuggets._
  import Nuggets.DomTypes._

  protected implicit val M: Monad[M]
  implicit val RefEquality: EqualK[Ref]
  protected implicit val Propagation: Propagation[M, Ref]
  implicit val Tracking: Tracking[M, Ref]

  def IncSets: nutcracker.IncSets[M, Ref]
  def PhosphoTargetOps: PhosphoTarget.Ops[M, Ref]
  def RuleOps: Rule.Ops[M, Ref]

  import Propagation._
  import Tracking._

  def addNuggets(
    rules: List[Rule[Ref]] = Nil,
    phosphoSites: List[PhosphoTriple[Ref]] = Nil
  ): M[Unit] = {
    import scalaz.syntax.traverse._
    val a = rules.traverse_(addRule(_) map (_ => ()))
    val b = phosphoSites.traverse_(pt => addPhosphoTarget(pt))
    M.apply2(a, b)((_, _) => ())
  }

  def addBinding(bnd: BindingData[Ref]): M[Binding[Ref]] =
    addRule(bnd.witness) map (ref => Binding(ref, bnd.link))

  def addBindings(bindings: List[BindingData[Ref]]): M[List[Binding[Ref]]] = {
    import scalaz.syntax.traverse._

    bindings traverse addBinding
  }

  // basic programs for adding nuggets
  def addRule(r: Rule[Ref]): M[Rule.Ref[Ref]] =
    newCell(Discrete(r)) >>! { track[λ[A[_] => Discrete[Rule[A]]]](_) }
  def addKinaseActivity(activeState: ProteinPattern[Ref]): M[Unit] =
    newCell(Discrete(activeState)) >>= { track[λ[A[_] => Discrete[ProteinPattern[A]]]](_) }

  // derived programs for adding nuggets
  def addPhosphoTarget(t: PhosphoTriple[Ref]): M[Unit] =
    for {
      pt <- PhosphoTargetOps.define(t)
      _ <- addRule(pt.witness)
    } yield ()

  // basic programs for querying nuggets
  def rules(f: Rule[Ref] => OnceTrigger[Rule.Ref[Ref] => M[Unit]]): M[Unit] =
    thresholdQuery[λ[Var[_] => Discrete[Rule[Var]]]](DomTypes.Rules)(r => f(r.value))
  def kinaseActivity(p: Protein)(f: ProteinPattern.Ref[Ref] => M[Unit]): M[Unit] =
    thresholdQuery[λ[Var[_] => Discrete[ProteinPattern[Var]]]](DomTypes.Kinases)(pp =>
      if(pp.value.protein === p) OnceTrigger.Fire(f)
      else OnceTrigger.Discard()
    )

  // queries in CPS style
  def rulesC(p: Rule[Ref] => OnceTrigger[Unit]): ContU[M, Rule.Ref[Ref]] =
    ContU(f => rules(p andThen (_.map(_ => f))))
  def kinaseActivityC(kinase: Protein): ContU[M, ProteinPattern.Ref[Ref]] =
    ContU(f => kinaseActivity(kinase)(f))

  // derived queries

  def bindingsOf(p: Protein)(f: Binding[Ref] => M[Unit]): M[Unit] =
    rules(_ => OnceTrigger.Fire(ruleRef => RuleOps.linksAgentToC(ruleRef)(p).apply(f)))
  def bindingsOfC(p: Protein): ContU[M, Binding[Ref]] =
    ContU(f => bindingsOf(p)(f))
  def bindingsOfS(p: Protein): M[Ref[IncSet[Binding[Ref]]]] =
    IncSets.collect(bindingsOfC(p))

  def phosphoTargets(f: PhosphoTarget.Ref[Ref] => M[Unit]): M[Unit] =
    rules(_ => OnceTrigger.Fire(ruleRef => RuleOps.phosphorylationsC(ruleRef).apply(f)))

  def phosphoSites(kinase: Protein, substrate: Protein)(f: ISite[Ref] => M[Unit]): M[Unit] =
    phosphoTargets(ptr => observe(ptr).by(apt => {
      val pt = apt.value
      if(kinase === pt.kinase && substrate === pt.substrate)
        Trigger.fireReload(f(pt.targetSite) map (_ => (d, δ) => ???))
      else
        Trigger.sleep((d, δ) => ???)
    }))
  def phosphoSitesC(kinase: Protein, substrate: Protein): ContU[M, ISite[Ref]] = // TODO: return Site.Ref
    ContU(f => phosphoSites(kinase, substrate)(f))
  def phosphoSitesS(kinase: Protein, substrate: Protein): M[Ref[IncSet[ISite[Ref]]]] =
    IncSets.collect(phosphoSitesC(kinase, substrate))

  def phosphoSitesS(substrate: Protein): M[Ref[IncSet[ISite[Ref]]]] =
    for {
      res <- IncSets.init[ISite[Ref]]
      _   <- phosphoTargets(ptr => observe(ptr).by(apt => {
        val pt = apt.value
        if(pt.substrate === substrate) Trigger.fireReload(IncSets.insert(pt.targetSite, res) map (_ => (d, δ) => ???))
        else Trigger.sleep((d, δ) => ???)
      }))
    } yield res
  def phosphoSitesC(substrate: Protein): ContU[M, ISite[Ref]] = // TODO: return Site.Ref
    phosphoSitesS(substrate).map(IncSets.forEach(_)).wrapEffect

  def kinasesOf(substrate: Protein, site: SiteLabel)(f: Protein => M[Unit]): M[Unit] =
    phosphoTargets(ptr => observe(ptr).by(apt => {
      val pt = apt.value
      if(substrate === pt.substrate && ISite[Ref](site) === pt.targetSite) Trigger.fireReload(f(pt.kinase) map (_ => (d, δ) => ???))
      else Trigger.sleep((d, δ) => ???)
    }))
  def kinasesOfC(substrate: Protein, site: SiteLabel): ContU[M, Protein] = // TODO: return Protein.Ref
    ContU(f => kinasesOf(substrate, site)(f))
  def kinasesOfS(substrate: Protein, site: SiteLabel): M[Ref[IncSet[Protein]]] =
    IncSets.collect(kinasesOfC(substrate, site))

  def forEachRule: ContU[M, Rule.Ref[Ref]] =
    rulesC(r => Fire(()))
}