package proteinrefinery.ui

import javafx.geometry.{Insets, Pos}
import javafx.scene.Node
import javafx.scene.control.{ComboBox, Label, Menu, MenuBar, MenuItem, ScrollPane, TextField}
import javafx.scene.layout.{Background, BackgroundFill, CornerRadii, GridPane, Region, StackPane, VBox}
import javafx.scene.paint.Color

import nutcracker.util.KMap
import nutcracker.{Antichain, DRef, Dom, IncSet}
import org.reactfx.value.Val
import org.reactfx.{EventSource, EventStream}
import proteinrefinery.lib.{Assoc, NegativeInfluenceOnPhosphorylation, Phosphorylation, Protein}
import proteinrefinery.ui.util.syntax._

import scalaz.Show

class GoalWidget {
  import GoalType._

  private var goals: KMap[GoalType, λ[A => List[GoalView[A]]]] = KMap[GoalType, λ[A => List[GoalView[A]]]]()

  private val newGoalMenu = new Menu("Add goal") <| { _.getItems.addAll(
    new MenuItem("Association") <| { _.onAction(() => showDialog(new AssocGoalInput)(pq => ReqGoalAssoc(pq._1, pq._2))) },
    new MenuItem("Phosphorylation") <| { _.onAction(() => showDialog(new PhosGoalInput)(ks => ReqGoalPhos(ks._1, ks._2))) },
    new MenuItem("Negative influence on phosphorylation") <| { _.onAction(() =>
      showDialog(new PhosNegInflInput(goals.get(GoalPhos).getOrElse(Nil)))(pgv => ReqGoalPhosNegInfl(pgv._1, pgv._2.ref, pgv._2.desc))
    ) }
  ).ignoreResult }

  private val goalBox = new VBox() <| {
    _.setFillWidth(true)
  }

  private val dialogHolder = new StackPane()

  val node = new ScrollPane(
    new VBox(
      new Label("Goals") <| { _.setStyle("-fx-font-weight: bold") },
      new MenuBar(newGoalMenu),
      dialogHolder,
      goalBox
    ) <| {
      _.setFillWidth(true) } <| {
      _.setAlignment(Pos.TOP_CENTER)
    }
  ) <| {
    _.setFitToWidth(true)
  }

  private val _requests = new EventSource[UIRequest]

  def requests: EventStream[UIRequest] = _requests

  def addView[A](view: GoalView[A]): Unit = {
    goals = goals.put(view.goalType)(view :: goals.getOrElse(view.goalType)(Nil))
    goalBox.getChildren.add(view.node)
    ()
  }

  def addSolution[A](t: GoalType[A], ref: DRef[IncSet[DRef[A]]], sref: DRef[A], sol: A)(implicit
    dom: Dom[A],
    show: Show[A]
  ): Unit = {
    withGoalView(t, ref)(_.addSolution(sref, sol))
  }

  def updateSolution[A](t: GoalType[A], ref: DRef[IncSet[DRef[A]]], sref: DRef[A], sol: A)(implicit
    dom: Dom[A],
    show: Show[A]
  ): Unit = {
    withGoalView(t, ref)(_.updateSolution(sref, sol))
  }

  private def withGoalView[A](t: GoalType[A], ref: DRef[IncSet[DRef[A]]])(f: GoalView[A] => Unit): Unit =
    goals.getOrElse(t)(Nil).find(_.ref == ref).foreach(f)

  private def showDialog[A](form: InputForm[A])(req: A => UIRequest): Unit =
    InputDialog.show(dialogHolder, form)(a => _requests.push(req(a)))

}

object GoalWidget {
  def apply(): GoalWidget = new GoalWidget

  private def setBackgroundColor(node: Region, color: Color): Unit =
    node.setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)))
}

trait GoalType[A] { self: Singleton => }
object GoalType {
  implicit object GoalAssoc extends GoalType[Antichain[Assoc[DRef]]]
  implicit object GoalPhos extends GoalType[Antichain[Phosphorylation[DRef]]]
  implicit object GoalPhosNegInfl extends GoalType[Antichain[NegativeInfluenceOnPhosphorylation[DRef]]]
}

class AssocGoalInput extends InputForm[(Protein, Protein)] {
  private val input1 = new TextField()
  private val input2 = new TextField()

  val title = "Association"

  val node = new GridPane() <| {
    _.addRow(0, new Label("Protein 1"), input1) } <| {
    _.addRow(1, new Label("Protein 2"), input2)
  }

  val input: Val[(Protein, Protein)] = (input1.textProperty() |@| input2.textProperty()).tuple
    .filter2[String, String]((p1, p2) => !p1.isEmpty && !p2.isEmpty)
    .map2[String, String, (Protein, Protein)]((p1, p2) => (Protein(p1), Protein(p2)))
}

class PhosGoalInput extends InputForm[(Protein, Protein)] {
  private val input1 = new TextField()
  private val input2 = new TextField()

  val title = "Phosphorylation"

  val node = new GridPane() <| {
    _.addRow(0, new Label("Kinase"), input1) } <| {
    _.addRow(1, new Label("Substrate"), input2)
  }

  val input: Val[(Protein, Protein)] = (input1.textProperty() |@| input2.textProperty()).tuple
    .filter2[String, String]((p1, p2) => !p1.isEmpty && !p2.isEmpty)
    .map2[String, String, (Protein, Protein)]((p1, p2) => (Protein(p1), Protein(p2)))
}

class PhosNegInflInput(phosGoals: List[GoalView[Antichain[Phosphorylation[DRef]]]]) extends InputForm[(Protein, GoalView[Antichain[Phosphorylation[DRef]]])] {
  private case class GVWrapper(goalView: GoalView[Antichain[Phosphorylation[DRef]]]) {
    override def toString = goalView.desc
  }

  private val agentInput = new TextField
  private val goalSelection = new ComboBox[GVWrapper] <| { cb =>
    phosGoals.foreach(gv => cb.getItems.add(GVWrapper(gv)))
  }

  val title: String = "Negative influence on phosphorylation"

  val node: Node = new GridPane() <| {
    _.addRow(0, new Label("Agent"), agentInput) } <| {
    _.addRow(1, new Label("Phosphorylation"), goalSelection)
  }

  val input: Val[(Protein, GoalView[Antichain[Phosphorylation[DRef]]])] = (agentInput.textProperty() |@| goalSelection.getSelectionModel.selectedItemProperty()).tuple
    .filter2[String, GVWrapper]((a, gv) => !a.isEmpty && gv != null)
    .map2[String, GVWrapper, (Protein, GoalView[Antichain[Phosphorylation[DRef]]])]((a, gv) => (Protein(a), gv.goalView))
}