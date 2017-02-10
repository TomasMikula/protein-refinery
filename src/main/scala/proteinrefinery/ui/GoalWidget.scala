package proteinrefinery.ui

import javafx.geometry.{Insets, Pos}
import javafx.scene.Node
import javafx.scene.control.{ComboBox, Label, Menu, MenuBar, MenuItem, ScrollPane, TextField}
import javafx.scene.layout.{Background, BackgroundFill, CornerRadii, GridPane, Region, StackPane, VBox}
import javafx.scene.paint.Color

import nutcracker.util.HHKMap
import nutcracker.{Discrete, Dom, IncSet}
import org.reactfx.value.Val
import org.reactfx.{EventSource, EventStream}
import proteinrefinery.lib.{Assoc, NegativeInfluenceOnPhosphorylation, PhosphoTarget, Protein}
import proteinrefinery.ui.util.syntax._

import scalaz.Show

class GoalWidget[Ref[_]] {
  import GoalType._

  private var goals: HHKMap[GoalType, λ[`A[_[_]]` => List[GoalView[Ref, A]]]] = HHKMap[GoalType, λ[`A[_[_]]` => List[GoalView[Ref, A]]]]()

  private val newGoalMenu = new Menu("Add goal") <| { _.getItems.addAll(
    new MenuItem("Association") <| { _.onAction(() => showDialog(new AssocGoalInput)(pq => ReqGoalAssoc(pq._1, pq._2))) },
    new MenuItem("Phosphorylation") <| { _.onAction(() => showDialog(new PhosGoalInput)(ks => ReqGoalPhos(ks._1, ks._2))) },
    new MenuItem("Negative influence on phosphorylation") <| { _.onAction(() =>
      showDialog(new PhosNegInflInput[Ref](goals.get[λ[`Ref[_]`=> Discrete[PhosphoTarget[Ref]]]](GoalPhos).getOrElse(Nil)))(pgv => ReqGoalPhosNegInfl(pgv._1, pgv._2.ref, pgv._2.desc))
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

  private val _requests = new EventSource[UIRequest[Ref]]

  def requests: EventStream[UIRequest[Ref]] = _requests

  def addView[A[_[_]]](view: GoalView[Ref, A]): Unit = {
    goals = goals.put(view.goalType)(view :: goals.getOrElse(view.goalType)(Nil))
    goalBox.getChildren.add(view.node)
    ()
  }

  def addSolution[A[_[_]]](t: GoalType[A], ref: Ref[IncSet[Ref[A[Ref]]]], sref: Ref[A[Ref]], sol: A[Ref])(implicit
    dom: Dom[A[Ref]],
    show: Show[A[Ref]]
  ): Unit = {
    withGoalView(t, ref)(_.addSolution(sref, sol))
  }

  def updateSolution[A[_[_]]](t: GoalType[A], ref: Ref[IncSet[Ref[A[Ref]]]], sref: Ref[A[Ref]], sol: A[Ref])(implicit
    dom: Dom[A[Ref]],
    show: Show[A[Ref]]
  ): Unit = {
    withGoalView(t, ref)(_.updateSolution(sref, sol))
  }

  private def withGoalView[A[_[_]]](t: GoalType[A], ref: Ref[IncSet[Ref[A[Ref]]]])(f: GoalView[Ref, A] => Unit): Unit =
    goals.getOrElse(t)(Nil).find(_.ref == ref).foreach(f)

  private def showDialog[A](form: InputForm[A])(req: A => UIRequest[Ref]): Unit =
    InputDialog.show(dialogHolder, form)(a => _requests.push(req(a)))

}

object GoalWidget {
  def apply[Ref[_]](): GoalWidget[Ref] = new GoalWidget

  private def setBackgroundColor(node: Region, color: Color): Unit =
    node.setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)))
}

trait GoalType[A[_[_]]] { self: Singleton =>
  type Data[Ref[_]] = A[Ref]
}
object GoalType {
  implicit object GoalAssoc extends GoalType[λ[`Ref[_]`=> Discrete[Assoc[Ref]]]]
  implicit object GoalPhos extends GoalType[λ[`Ref[_]`=> Discrete[PhosphoTarget[Ref]]]]
  implicit object GoalPhosNegInfl extends GoalType[λ[`Ref[_]`=> Discrete[NegativeInfluenceOnPhosphorylation[Ref]]]]
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

class PhosNegInflInput[Ref[_]](phosGoals: List[GoalView[Ref, λ[`Var[_]` => Discrete[PhosphoTarget[Var]]]]]) extends InputForm[(Protein, GoalView[Ref, λ[`Var[_]` => Discrete[PhosphoTarget[Var]]]])] {
  private case class GVWrapper(goalView: GoalView[Ref, λ[`Var[_]` => Discrete[PhosphoTarget[Var]]]]) {
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

  val input: Val[(Protein, GoalView[Ref, λ[`Var[_]` => Discrete[PhosphoTarget[Var]]]])] =
    (agentInput.textProperty() |@| goalSelection.getSelectionModel.selectedItemProperty()).tuple
      .filter2[String, GVWrapper]((a, gv) => !a.isEmpty && gv != null)
      .map2[String, GVWrapper, (Protein, GoalView[Ref, λ[`Var[_]` => Discrete[PhosphoTarget[Var]]]])]((a, gv) => (Protein(a), gv.goalView))
}