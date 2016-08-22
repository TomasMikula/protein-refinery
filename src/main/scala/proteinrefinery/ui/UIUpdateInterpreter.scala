package proteinrefinery.ui

import scala.language.higherKinds
import nutcracker.util.≈>>
import proteinrefinery.ui.UIUpdateLang._

import scalaz.Id._

class UIUpdateInterpreter(kbWidget: KBWidget, goalWidget: GoalWidget) extends (UIUpdateLang ≈>> Id) {
  def apply[K[_], A](u: UIUpdateLang[K, A]): A = u match {
    case InitGoal(gt, ref, desc) => goalWidget.addView(GoalView.init(gt, desc, ref))
    case AddSolution(gt, ref, sref, sol, dom, show) => goalWidget.addSolution(gt, ref, sref, sol)(dom, show)
    case UpdateSolution(gt, ref, sref, sol, dom, show) => goalWidget.updateSolution(gt, ref, sref, sol)(dom, show)
    case NewFact(t, fact, show) => kbWidget.factAdded(t, fact)(show)
  }
}

object UIUpdateInterpreter {
  def apply(kbWidget: KBWidget, goalWidget: GoalWidget): UIUpdateInterpreter = new UIUpdateInterpreter(kbWidget, goalWidget)
}