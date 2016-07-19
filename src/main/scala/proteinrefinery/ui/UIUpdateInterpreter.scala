package proteinrefinery.ui

import scala.language.higherKinds
import nutcracker.util.≈>>
import proteinrefinery.ui.UIUpdateLang._

import scalaz.Id._

class UIUpdateInterpreter(kbWidget: KBWidget, goalWidget: GoalWidget) extends (UIUpdateLang ≈>> Id) {
  def apply[K[_], A](u: UIUpdateLang[K, A]): A = u match {
    case TrackGoal(gt, desc, ref, value, show) => goalWidget.addView(GoalView.init(gt, desc, ref, value.value)(show))
    case GoalUpdated(t, ref, newVal, delta) => goalWidget.updateView(t, ref, newVal, delta)
    case NewFact(t, fact, show) => kbWidget.factAdded(t, fact)(show)
  }
}

object UIUpdateInterpreter {
  def apply(kbWidget: KBWidget, goalWidget: GoalWidget): UIUpdateInterpreter = new UIUpdateInterpreter(kbWidget, goalWidget)
}