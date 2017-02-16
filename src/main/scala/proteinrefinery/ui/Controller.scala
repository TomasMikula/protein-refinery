package proteinrefinery.ui

import monocle.Lens
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.CoproductK.:++:
import nutcracker.util.{DeepShow, FreeK}
import nutcracker.{Diff, Discrete, Dom, IncSet, Propagation}
import nutcracker.Propagation.{module => Prop}
import org.reactfx.EventStreams
import proteinrefinery.lib.{Assoc, BindingData, ISite, NegativeInfluenceOnPhosphorylation, PhosphoTarget, PhosphoTriple, Protein, ProteinPattern, SiteLabel}
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.ui.FactType._
import proteinrefinery.ui.util.syntax._

import scala.language.higherKinds
import scalaz.Id._
import scalaz.{Show, ~>}

class Controller(val kbWidget: KBWidget[Prop.Ref], val goalWidget: GoalWidget[Prop.Ref]) {
  import Controller._
  import Prop._

  val Propagation: Propagation[Prg, Ref] = Prop.propagation[DSL]
  val UIUpdate: UIUpdate[Prg, Ref] = UIUpdateLang.freeUIUpdate[Ref, DSL]
  val IncSets: nutcracker.IncSets[Prg, Ref] = new nutcracker.IncSets[Prg, Ref]()(Propagation)
  import Propagation._
  import UIUpdate._

  val interpreter = (UIUpdateInterpreter(kbWidget, goalWidget) :>>: proteinrefinery.interpreter).freeInstance
  var state = proteinrefinery.emptyState[Prg[Unit]]
  val fetch = λ[Ref ~> Id](ref => Prop.fetch(implicitly[Lens[proteinrefinery.State[Prg[Unit]], Prop.State[Prg[Unit]]]].get(state))(ref))

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

  private def addFactPhosSite(k: Protein, s: Protein, ss: SiteLabel): Prg[Unit] =
    Lib.addPhosphoTarget(PhosphoTriple(k, s, ISite(ss))) >> newFact(FactPhosTarget, PhosphoTarget(k, s, ISite[Ref](ss)))

  private def observeGoal[A[_[_]]](desc: String, ref: Ref[IncSet[Ref[A[Ref]]]])(implicit t: GoalType[A], dom: Dom[A[Ref]], show: Show[A[Ref]]): Prg[Unit] =
    observe(ref).by(d => {
      val now = initGoal(t, ref, desc)
      val onChange = (d: IncSet[Ref[A[Ref]]], δ: Diff[Set[Ref[A[Ref]]]]) => updateGoal[A](t, ref, δ)
      fireReload(now map (_ => continually(onChange)))
    })

  private def updateGoal[A[_[_]]](t: GoalType[A], gref: Ref[IncSet[Ref[A[Ref]]]], δ: Diff[Set[Ref[A[Ref]]]])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): Prg[Unit] =
    FreeK.sequence_(δ.value.iterator.map(observeSolution[A](t, gref, _)).toList)

  private def observeSolution[A[_[_]]](t: GoalType[A], gref: Ref[IncSet[Ref[A[Ref]]]], sref: Ref[A[Ref]])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): Prg[Unit] =
    observe(sref).by(a => {
      val now = addSolution[A](t, gref, sref, a)
      val onChange = (a: A[Ref], δ: dom.Delta) => updateSolution[A](t, gref, sref, a)
      fireReload(now map (_ => continually(onChange)))
    })
}

object Controller {
  import Prop._

  type DSL[K[_], A] = (UIUpdateLang[Ref, ?[_], ?] :++: proteinrefinery.DSL)#Out[K, A]
  type Prg[A] = FreeK[DSL, A]

  val Lib = new proteinrefinery.Lib[Prg, Ref]

  def apply(kbWidget: KBWidget[Ref], goalWidget: GoalWidget[Ref]): Controller =
    new Controller(kbWidget, goalWidget)
}