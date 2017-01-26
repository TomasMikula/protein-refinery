package proteinrefinery.ui

import monocle.Lens
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.CoproductK.:++:
import nutcracker.util.{DeepShow, FreeK}
import nutcracker.{Discrete, DRef, Diff, Dom, IncSet, Propagation, PropagationLang, PropagationStore}
import org.reactfx.EventStreams
import proteinrefinery.lib.{BindingData, ISite, PhosphoTarget, PhosphoTriple, Protein, ProteinPattern, SiteLabel}
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.ui.FactType._
import proteinrefinery.ui.util.syntax._

import scala.language.higherKinds
import scalaz.Id._
import scalaz.{Show, ~>}

class Controller(val kbWidget: KBWidget, val goalWidget: GoalWidget) {
  import Controller._

  val Propagation: Propagation[Prg, DRef] = PropagationLang.freePropagation[DSL]
  val UIUpdate: UIUpdate[Prg, DRef] = UIUpdateLang.freeUIUpdate[DSL]
  val IncSets: nutcracker.IncSets[Prg, DRef] = new nutcracker.IncSets[Prg, DRef]()(Propagation)
  import Propagation._
  import UIUpdate._

  val interpreter = (UIUpdateInterpreter(kbWidget, goalWidget) :>>: proteinrefinery.interpreter).freeInstance
  var state = proteinrefinery.emptyState[Prg[Unit]]
  val fetch = λ[DRef ~> Id](ref => implicitly[Lens[proteinrefinery.State[Prg[Unit]], PropagationStore[Prg[Unit]]]].get(state).fetch(ref))

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
      observeGoal(s"Association between $p and $q", _)
    }

  private def addGoalPhos(kinase: Protein, substrate: Protein): Prg[Unit] =
    Lib.phosphorylations(kinase, substrate) >>= { observeGoal(s"Phosphorylation of $substrate by $kinase", _) }

  private def addGoalPhosNegInfl(agent: Protein, phosGoal: DRef[IncSet[DRef[Discrete[PhosphoTarget[DRef]]]]], phosDesc: String): Prg[Unit] =
    IncSets.relBind(phosGoal)(phRef => Lib.negativeInfluenceOnPhosphorylation_r(agent, phRef)) >>= {
      observeGoal(s"Negative influence of $agent on $phosDesc", _)
    }

  private def addFactBind(p: Protein, ps: SiteLabel, q: Protein, qs: SiteLabel): Prg[Unit] = {
    val rule = BindingData(p, LocalSiteId[DRef](ps), q, LocalSiteId[DRef](qs)).witness
    Lib.addRule(rule) >>= (_ => newFact(FactRule, rule))
  }

  private def addFactKinase(pp: ProteinPattern[DRef]): Prg[Unit] = {
    Lib.addKinaseActivity(pp) >> newFact(FactKinase, pp)(DeepShow.show[DRef, ProteinPattern[DRef]](fetch))
  }

  private def addFactPhosSite(k: Protein, s: Protein, ss: SiteLabel): Prg[Unit] =
    Lib.addPhosphoTarget(PhosphoTriple(k, s, ISite(ss))) >> newFact(FactPhosTarget, PhosphoTarget(k, s, ISite[DRef](ss)))

  private def observeGoal[A](desc: String, ref: DRef[IncSet[DRef[A]]])(implicit t: GoalType[A], dom: Dom[A], show: Show[A]): Prg[Unit] =
    observe(ref).by(d => {
      val now = initGoal(t, ref, desc)
      val onChange = (d: IncSet[DRef[A]], δ: Diff[Set[DRef[A]]]) => fireReload(updateGoal[A](t, ref, δ))
      (Some(now), Some(onChange))
    })

  private def updateGoal[A](t: GoalType[A], gref: DRef[IncSet[DRef[A]]], δ: Diff[Set[DRef[A]]])(implicit dom: Dom[A], show: Show[A]): Prg[Unit] =
    FreeK.sequence_(δ.value.iterator.map(observeSolution[A](t, gref, _)).toList)

  private def observeSolution[A](t: GoalType[A], gref: DRef[IncSet[DRef[A]]], sref: DRef[A])(implicit dom: Dom[A], show: Show[A]): Prg[Unit] =
    observe(sref).by(a => {
      val now = addSolution[A](t, gref, sref, a)
      val onChange = (a: A, δ: dom.Delta) => fireReload(updateSolution[A](t, gref, sref, a))
      (Some(now), Some(onChange))
    })
}

object Controller {

  type DSL[K[_], A] = (UIUpdateLang :++: proteinrefinery.DSL)#Out[K, A]
  type Prg[A] = FreeK[DSL, A]

  val Lib = new proteinrefinery.Lib[Prg, DRef]

  def apply(kbWidget: KBWidget, goalWidget: GoalWidget): Controller =
    new Controller(kbWidget, goalWidget)
}