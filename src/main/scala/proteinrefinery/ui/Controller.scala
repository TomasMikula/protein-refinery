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
import scalaz.{Show, ~>}
import scalaz.Id._
import scalaz.syntax.monad._

class Controller(val kbWidget: KBWidget[Controller.Var], val goalWidget: GoalWidget[Controller.Var]) {
  import Controller._
  import refinery.{refEquality, refShow}

  val UIUpdate: UIUpdate[Prg, Var] = UIUpdateLang.freeUIUpdate[Var, DSL]
  val IncSets: nutcracker.IncSets[Prg, Var, Val] = new nutcracker.IncSets[Prg, Var, Val]()(Controller.Propagation)
  import Controller.Propagation.{Val => _, _}
  import UIUpdate._

  val interpreter = (UIUpdateInterpreter(kbWidget, goalWidget) :>>: refinery.interpreter).freeInstance
  var state = refinery.empty[Prg]
  val fetch = λ[Var ~> Id](ref => refinery.fetch(ref, state))

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

  private def addGoalPhosNegInfl(agent: Protein, phosGoal: Var[IncSet[Var[Discrete[PhosphoTarget[Var]]]]], phosDesc: String): Prg[Unit] =
    IncSets.relBind(phosGoal)(phRef => Lib.negativeInfluenceOnPhosphorylation_r(agent, phRef)) >>= {
      observeGoal[λ[`Var[_]` => Discrete[NegativeInfluenceOnPhosphorylation[Var]]]](s"Negative influence of $agent on $phosDesc", _)
    }

  private def addFactBind(p: Protein, ps: SiteLabel, q: Protein, qs: SiteLabel): Prg[Unit] = {
    val rule = BindingData(p, LocalSiteId[Var](ps), q, LocalSiteId[Var](qs)).witness
    Lib.addRule(rule) >>= (_ => newFact(FactRule, rule))
  }

  private def addFactKinase(pp: ProteinPattern[Var]): Prg[Unit] = {
    Lib.addKinaseActivity(pp) >> newFact(FactKinase, pp)(DeepShow.show[Var, ProteinPattern[Var]](fetch))
  }

  private def addFactPhosSite(k: Protein, s: Protein, ss: SiteLabel): Prg[Unit] = for {
    _ <- Lib.addPhosphoTarget(PhosphoTriple(k, s, ISite(ss)))
    u <- newFact(FactPhosTarget, PhosphoTarget(k, s, ISite[Var](ss)))
  } yield u

  private def observeGoal[A[_[_]]](desc: String, ref: Var[IncSet[Var[A[Var]]]])(implicit t: GoalType[A], dom: Dom[A[Var]], show: Show[A[Var]]): Prg[Unit] =
    observe(ref).by_(d => {
      val now = initGoal(t, ref, desc)
      val onChange = (d: IncSet[Var[A[Var]]], δ: IncSet.Delta[Var[A[Var]]]) => updateGoal[A](t, ref, δ)
      fireReload(now.as(sleep(continually(onChange))))
    })

  private def updateGoal[A[_[_]]](t: GoalType[A], gref: Var[IncSet[Var[A[Var]]]], δ: IncSet.Delta[Var[A[Var]]])(implicit dom: Dom[A[Var]], show: Show[A[Var]]): Prg[Unit] =
    δ.value.iterator.traverse_(observeSolution[A](t, gref, _))

  private def observeSolution[A[_[_]]](t: GoalType[A], gref: Var[IncSet[Var[A[Var]]]], sref: Var[A[Var]])(implicit dom: Dom[A[Var]], show: Show[A[Var]]): Prg[Unit] =
    observe(sref).by_(a => {
      val now = addSolution[A](t, gref, sref, a)
      val onChange = (a: A[Var], δ: dom.Delta) => updateSolution[A](t, gref, sref, a)
      fireReload(now.as(sleep(continually(onChange))))
    })
}

object Controller {
  val refinery = proteinrefinery.refinery()
  type Var[A] = refinery.Var[A]
  type Val[A] = refinery.Val[A]

  type DSL[K[_], A] = (UIUpdateLang[Var, ?[_], ?] :++: refinery.Lang)#Out[K, A]
  type Prg[A] = FreeK[DSL, A]

  implicit val Propagation: Propagation[Prg, Var, Val] = refinery.freePropagation[DSL]
  private implicit def deferApi: Defer[Prg, Cost] = refinery.freeDeferApi[DSL]
  private implicit def trackingApi: Tracking[Prg, Var, Val] = refinery.freeTrackingApi[DSL]
  import refinery.refEquality

  val Lib = new proteinrefinery.Lib[Prg, Var, Val]

  def apply(kbWidget: KBWidget[Var], goalWidget: GoalWidget[Var]): Controller =
    new Controller(kbWidget, goalWidget)
}