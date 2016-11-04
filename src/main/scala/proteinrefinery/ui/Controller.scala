package proteinrefinery.ui

import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.CoproductK.:++:
import nutcracker.util.FreeK
import nutcracker.{Antichain, DRef, Diff, Dom, IncSet, Propagation, PropagationLang}
import org.reactfx.EventStreams
import proteinrefinery.lib.{Binding, Phosphorylation, Protein, ProteinPattern, SiteLabel}
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.ui.FactType._
import proteinrefinery.ui.UIUpdateLang._
import proteinrefinery.ui.util.syntax._

import scala.language.higherKinds
import scalaz.Show
import scalaz.std.tuple._

class Controller(val kbWidget: KBWidget, val goalWidget: GoalWidget) {
  import Controller._

  val Propagation: Propagation[Prg, DRef] = PropagationLang.freePropagation[DSL]
  val IncSets: nutcracker.IncSets[Prg, DRef] = new nutcracker.IncSets[Prg, DRef]()(Propagation)
  import Propagation._

  val interpreter = (UIUpdateInterpreter(kbWidget, goalWidget) :>>: proteinrefinery.interpreter).freeInstance
  var state = proteinrefinery.emptyState[Prg[Unit]]

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
    Lib.assoc(p, q).inject[DSL] >>= {
      observeGoal(s"Association between $p and $q", _)
    }

  private def addGoalPhos(kinase: Protein, substrate: Protein): Prg[Unit] =
    Lib.phosphorylation(kinase, substrate).inject[DSL] >>= {
      observeGoal(s"Phosphorylation of $substrate by $kinase", _)
    }

  private def addGoalPhosNegInfl(agent: Protein, phosGoal: DRef[IncSet[DRef[Antichain[Phosphorylation[DRef]]]]], phosDesc: String): Prg[Unit] =
    IncSets.relBind(phosGoal)(phRef => Lib.negativeInfluenceOnPhosphorylation_r(agent, phRef.infer)) >>= {
      observeGoal(s"Negative influence of $agent on $phosDesc", _)
    }

  private def addFactBind(p: Protein, ps: SiteLabel, q: Protein, qs: SiteLabel): Prg[Unit] = {
    val rule = Binding[DRef](p, LocalSiteId(ps), q, LocalSiteId(qs)).witness
    Lib.addRuleF(rule) >> newFactF(FactRule, rule)
  }

  private def addFactKinase(pp: ProteinPattern[DRef]): Prg[Unit] = {
    Lib.addKinaseActivityF(pp) >> newFactF(FactKinase, pp)
  }

  private def addFactPhosSite(k: Protein, s: Protein, ss: SiteLabel): Prg[Unit] =
    Lib.addPhosphoTargetF(k, s, ss) >> newFactF(FactPhosTarget, (k, s, ss))

  private def observeGoal[A](desc: String, ref: DRef[IncSet[DRef[A]]])(implicit t: GoalType[A], dom: Dom[A], show: Show[A]): Prg[Unit] =
    observe(ref).by(d => {
      val now = initGoalF(t, ref, desc).inject[DSL]
      val onChange = (d: IncSet[DRef[A]], δ: Diff[Set[DRef[A]]]) => fireReload(updateGoal[A](t, ref, δ))
      (Some(now), Some(onChange))
    })

  private def updateGoal[A](t: GoalType[A], gref: DRef[IncSet[DRef[A]]], δ: Diff[Set[DRef[A]]])(implicit dom: Dom[A], show: Show[A]): Prg[Unit] =
    FreeK.sequence_(δ.value.iterator.map(observeSolution[A](t, gref, _)).toList)

  private def observeSolution[A](t: GoalType[A], gref: DRef[IncSet[DRef[A]]], sref: DRef[A])(implicit dom: Dom[A], show: Show[A]): Prg[Unit] =
    observe(sref).by(a => {
      val now = addSolutionF[DSL, A](t, gref, sref, a)
      val onChange = (a: A, δ: dom.Delta) => fireReload(updateSolutionF[DSL, A](t, gref, sref, a))
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