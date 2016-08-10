package proteinrefinery.ui

import nutcracker.IncSet.IncSetRef
import nutcracker.{Diff, IncSet}
import nutcracker.PropagationLang._

import scala.language.higherKinds
import nutcracker.Trigger._
import nutcracker.util.FreeK
import nutcracker.util.CoproductK.:++:
import org.reactfx.EventStreams
import proteinrefinery.lib.{Assoc, Binding, KB, NegativeInfluenceOnPhosphorylation, Phosphorylation, Protein, ProteinPattern, Site}
import proteinrefinery.ui.FactType._
import proteinrefinery.ui.UIUpdateLang._
import proteinrefinery.ui.util.syntax._

import scalaz.Show
import scalaz.std.tuple._

class Controller(val kbWidget: KBWidget, val goalWidget: GoalWidget) {
  import Controller._

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
    Assoc.search(p, q).inject[DSL] >>= {
      observeGoal(s"Association between $p and $q", _)
    }

  private def addGoalPhos(kinase: Protein, substrate: Protein): Prg[Unit] =
    Phosphorylation.search(kinase, substrate).inject[DSL] >>= {
      observeGoal(s"Phosphorylation of $substrate by $kinase", _)
    }

  private def addGoalPhosNegInfl(agent: Protein, phosGoal: IncSetRef[Phosphorylation], phosDesc: String): Prg[Unit] =
    IncSet.relBind(phosGoal)(ph => NegativeInfluenceOnPhosphorylation.search(agent, ph)).inject[DSL] >>= {
      observeGoal(s"Negative influence of $agent on $phosDesc", _)
    }

  private def addFactBind(p: Protein, ps: Site, q: Protein, qs: Site): Prg[Unit] = {
    val rule = Binding(p, ps, q, qs).witness
    KB.addRuleF[DSL](rule) >> newFactF(FactRule, rule)
  }

  private def addFactKinase(pp: ProteinPattern): Prg[Unit] = {
    KB.addKinaseActivityF[DSL](pp) >> newFactF(FactKinase, pp)
  }

  private def addFactPhosSite(k: Protein, s: Protein, ss: Site): Prg[Unit] =
    KB.addPhosphoTargetF[DSL](k, s, ss) >> newFactF(FactPhosTarget, (k, s, ss))

  private def observeGoal[A: Show](desc: String, ref: IncSetRef[A])(implicit t: GoalType[A]): Prg[Unit] =
    domTriggerF(ref)(d => {
      val now = trackGoalF(t, desc, ref, d).inject[DSL]
      val onChange = (d: IncSet[A], δ: Diff[Set[A]]) => fireReload(goalUpdatedF(t, ref, d, δ).inject[DSL])
      (Some(now), Some(onChange))
    })
}

object Controller {

  type DSL[K[_], A] = (UIUpdateLang :++: proteinrefinery.DSL)#Out[K, A]
  type Prg[A] = FreeK[DSL, A]

  def apply(kbWidget: KBWidget, goalWidget: GoalWidget): Controller =
    new Controller(kbWidget, goalWidget)
}