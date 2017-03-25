package proteinrefinery.ui

import nutcracker.{Defer, Discrete, Dom, IncSet, Propagation}
import nutcracker.Trigger._
import nutcracker.util.CoproductK.:++:
import nutcracker.util.{DeepShow, FreeK}
import nutcracker.util.ops._
import org.reactfx.EventStreams
import proteinrefinery.Cost
import proteinrefinery.lib.{Assoc, BindingData, ISite, NegativeInfluenceOnPhosphorylation, PhosphoTarget, PhosphoTriple, Protein, ProteinPattern, SiteLabel}
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.ui.FactType._
import proteinrefinery.ui.util.syntax._
import proteinrefinery.util.Tracking
import scala.language.higherKinds
import scalaz.Id._
import scalaz.{Show, ~>}

class Controller(val kbWidget: KBWidget[Controller.Ref], val goalWidget: GoalWidget[Controller.Ref]) {
  import Controller._
  import refinery.{refEquality, refShow}

  val UIUpdate: UIUpdate[Prg, Ref] = UIUpdateLang.freeUIUpdate[Ref, DSL]
  val IncSets: nutcracker.IncSets[Prg, Ref] = new nutcracker.IncSets[Prg, Ref]()(Controller.Propagation)
  import Controller.Propagation._
  import UIUpdate._

  val interpreter = (UIUpdateInterpreter(kbWidget, goalWidget) :>>: refinery.interpreter).freeInstance
  var state = refinery.empty[Prg]
  val fetch = λ[Ref ~> Id](ref => refinery.fetch(ref, state))

  EventStreams.merge(kbWidget.requests, goalWidget.requests).forEach(_ match {
    case ReqGoalAssoc(p, q) => exec(addGoalAssoc(p, q))
    case ReqGoalPhos(k, s) => exec(addGoalPhos(k, s))
    case ReqGoalPhosNegInfl(agent, phosGoal, phosDesc) => exec(addGoalPhosNegInfl(agent, phosGoal, phosDesc))

    case ReqAssertBind(p, ps, q, qs) => exec(addFactBind(p, ps, q, qs))
    case ReqAssertKinaseActivity(pp) => exec(addFactKinase(pp))
    case ReqAssertPhosSite(k, s, ss) => exec(addFactPhosSite(k, s, ss))
  })

  private def exec[A](prg: Prg[A]): A = interpreter(prg).run(state) match {
    case (s, a) => state = s; a
  }

  private def addGoalAssoc(p: Protein, q: Protein): Prg[Unit] =
    Lib.assoc(p, q) >>= {
      observeGoal[λ[`Var[_]` => Discrete[Assoc[Var]]]](s"Association between $p and $q", _)
    }

  private def addGoalPhos(kinase: Protein, substrate: Protein): Prg[Unit] =
    Lib.phosphorylations(kinase, substrate) >>= {
      observeGoal[λ[`Var[_]` => Discrete[PhosphoTarget[Var]]]](s"Phosphorylation of $substrate by $kinase", _)
    }

  private def addGoalPhosNegInfl(agent: Protein, phosGoal: Ref[IncSet[Ref[Discrete[PhosphoTarget[Ref]]]]], phosDesc: String): Prg[Unit] =
    IncSets.relBind(phosGoal)(phRef => Lib.negativeInfluenceOnPhosphorylation_r(agent, phRef)) >>= {
      observeGoal[λ[`Var[_]` => Discrete[NegativeInfluenceOnPhosphorylation[Var]]]](s"Negative influence of $agent on $phosDesc", _)
    }

  private def addFactBind(p: Protein, ps: SiteLabel, q: Protein, qs: SiteLabel): Prg[Unit] = {
    val rule = BindingData(p, LocalSiteId[Ref](ps), q, LocalSiteId[Ref](qs)).witness
    Lib.addRule(rule) >>= (_ => newFact(FactRule, rule))
  }

  private def addFactKinase(pp: ProteinPattern[Ref]): Prg[Unit] = {
    Lib.addKinaseActivity(pp) >> newFact(FactKinase, pp)(DeepShow.show[Ref, ProteinPattern[Ref]](fetch))
  }

  private def addFactPhosSite(k: Protein, s: Protein, ss: SiteLabel): Prg[Unit] = for {
    _ <- Lib.addPhosphoTarget(PhosphoTriple(k, s, ISite(ss)))
    u <- newFact(FactPhosTarget, PhosphoTarget(k, s, ISite[Ref](ss)))
  } yield u

  private def observeGoal[A[_[_]]](desc: String, ref: Ref[IncSet[Ref[A[Ref]]]])(implicit t: GoalType[A], dom: Dom[A[Ref]], show: Show[A[Ref]]): Prg[Unit] =
    observe(ref).by_(d => {
      val now = initGoal(t, ref, desc)
      val onChange = (d: IncSet[Ref[A[Ref]]], δ: IncSet.Delta[Ref[A[Ref]]]) => updateGoal[A](t, ref, δ)
      fireReload(now map (_ => continually(onChange)))
    })

  private def updateGoal[A[_[_]]](t: GoalType[A], gref: Ref[IncSet[Ref[A[Ref]]]], δ: IncSet.Delta[Ref[A[Ref]]])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): Prg[Unit] =
    δ.value.iterator.traverse_(observeSolution[A](t, gref, _))

  private def observeSolution[A[_[_]]](t: GoalType[A], gref: Ref[IncSet[Ref[A[Ref]]]], sref: Ref[A[Ref]])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): Prg[Unit] =
    observe(sref).by_(a => {
      val now = addSolution[A](t, gref, sref, a)
      val onChange = (a: A[Ref], δ: dom.Delta) => updateSolution[A](t, gref, sref, a)
      fireReload(now map (_ => continually(onChange)))
    })
}

object Controller {
  val refinery = proteinrefinery.refinery()
  type Ref[A] = refinery.Ref[A]

  type DSL[K[_], A] = (UIUpdateLang[Ref, ?[_], ?] :++: refinery.Lang)#Out[K, A]
  type Prg[A] = FreeK[DSL, A]

  implicit val Propagation: Propagation[Prg, Ref] = refinery.freePropagation[DSL]
  private implicit def deferApi: Defer[Prg, Cost] = refinery.freeDeferApi[DSL]
  private implicit def trackingApi: Tracking[Prg, Ref] = refinery.freeTrackingApi[DSL]
  import refinery.refEquality

  val Lib = new proteinrefinery.Lib[Prg, Ref]

  def apply(kbWidget: KBWidget[Ref], goalWidget: GoalWidget[Ref]): Controller =
    new Controller(kbWidget, goalWidget)
}