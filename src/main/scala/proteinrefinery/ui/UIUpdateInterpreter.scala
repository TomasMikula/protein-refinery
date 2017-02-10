package proteinrefinery.ui

import scala.language.higherKinds
import nutcracker.util.≈>>
import proteinrefinery.ui.UIUpdateLang._

import scalaz.Id._

class UIUpdateInterpreter[Ref[_]](kbWidget: KBWidget[Ref], goalWidget: GoalWidget[Ref]) extends (UIUpdateLang[Ref, ?[_], ?] ≈>> Id) {
  def apply[K[_], A](u: UIUpdateLang[Ref, K, A]): A = u match {
    case InitGoal(gt, ref, desc) => goalWidget.addView(GoalView.init[Ref, gt.Data](gt, desc, ref))
    case AddSolution(gt, ref, sref, sol, dom, show) => goalWidget.addSolution[gt.Data](gt, ref, sref, sol)(dom, show)
    case UpdateSolution(gt, ref, sref, sol, dom, show) => goalWidget.updateSolution[gt.Data](gt, ref, sref, sol)(dom, show)
    case NewFact(t, fact, show) => kbWidget.factAdded[t.Data](t, fact)(show)
  }
}

object UIUpdateInterpreter {
  def apply[Ref[_]](kbWidget: KBWidget[Ref], goalWidget: GoalWidget[Ref]): UIUpdateInterpreter[Ref] =
    new UIUpdateInterpreter(kbWidget, goalWidget)
}