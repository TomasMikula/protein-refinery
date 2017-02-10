package proteinrefinery.ui

import javafx.scene.Node
import javafx.scene.control.TitledPane
import javafx.scene.layout.VBox
import javafx.scene.text.Text

import nutcracker.IncSet
import proteinrefinery.ui.util.syntax._

import scalaz.Show

class GoalView[Ref[_], A[_[_]]](val goalType: GoalType[A], val desc: String, val ref: Ref[IncSet[Ref[A[Ref]]]]) {

  private val solutionNodes = new VBox()

  private var nodesBySolution = Map[Ref[A[Ref]], Text]()

  val node = new TitledPane(desc, solutionNodes)

  def addSolution(ref: Ref[A[Ref]], a: A[Ref])(implicit A: Show[A[Ref]]): Unit = {
    val node = new Text(A.shows(a))
    solutionNodes.getChildren().add(node)
    nodesBySolution = nodesBySolution.updated(ref, node)
  }

  def updateSolution(ref: Ref[A[Ref]], a: A[Ref])(implicit A: Show[A[Ref]]): Unit = {
    val node = nodesBySolution(ref)
    node.setText(A.shows(a))
  }

  private def removeSolutionNode(node: Node): Unit =
    solutionNodes.getChildren.remove(node).ignoreResult()
}

object GoalView {

  def init[Ref[_], A[_[_]]](gt: GoalType[A], desc: String, ref: Ref[IncSet[Ref[A[Ref]]]]): GoalView[Ref, A] =
    new GoalView[Ref, A](gt, desc, ref)

}