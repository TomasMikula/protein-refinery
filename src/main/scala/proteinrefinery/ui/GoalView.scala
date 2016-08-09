package proteinrefinery.ui

import javafx.scene.control.TitledPane
import javafx.scene.layout.VBox
import javafx.scene.text.Text

import nutcracker.{Diff, IncSet}
import nutcracker.IncSet.IncSetRef
import proteinrefinery.ui.util.syntax._

import scalaz.Show

class GoalView[A](val goalType: GoalType[A], val desc: String, val ref: IncSetRef[A])(implicit A: Show[A]) {

  private val goals = new VBox()

  val node = new TitledPane(desc, goals)

  def updateSolutions(newVal: IncSet[A], delta: Diff[Set[A]]): Unit = { // linter:ignore UnusedParameter
    delta.value.foreach(a => addSolution(a))
  }

  private def addSolution(a: A)(implicit A: Show[A]): Unit = {
    goals.getChildren().add(new Text(A.shows(a))).ignoreResult
  }
}

object GoalView {
  def init[A: Show](gt: GoalType[A], desc: String, ref: IncSetRef[A], initialItems: Iterable[A]): GoalView[A] = {
    new GoalView[A](gt, desc, ref) <| { gv =>
      initialItems.foreach(gv.addSolution(_))
    }
  }
}