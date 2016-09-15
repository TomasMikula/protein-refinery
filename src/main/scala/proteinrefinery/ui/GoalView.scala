package proteinrefinery.ui

import javafx.scene.Node
import javafx.scene.control.TitledPane
import javafx.scene.layout.VBox
import javafx.scene.text.Text

import nutcracker.DRef
import nutcracker.IncSet.IncSetRef
import proteinrefinery.ui.util.syntax._

import scalaz.Show

class GoalView[A](val goalType: GoalType[A], val desc: String, val ref: IncSetRef[_ <: DRef[A]]) {

  private val solutionNodes = new VBox()

  private var nodesBySolution = Map[DRef[A], Text]()

  val node = new TitledPane(desc, solutionNodes)

  def addSolution(ref: DRef[A], a: A)(implicit A: Show[A]): Unit = {
    val node = new Text(A.shows(a))
    solutionNodes.getChildren().add(node)
    nodesBySolution = nodesBySolution.updated(ref, node)
  }

  def updateSolution(ref: DRef[A], a: A)(implicit A: Show[A]): Unit = {
    val node = nodesBySolution(ref)
    node.setText(A.shows(a))
  }

  private def removeSolutionNode(node: Node): Unit =
    solutionNodes.getChildren.remove(node).ignoreResult()
}

object GoalView {

  def init[A](gt: GoalType[A], desc: String, ref: IncSetRef[_ <: DRef[A]]): GoalView[A] =
    new GoalView[A](gt, desc, ref)

}