package protein.ui

import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.control.{Label, ListView, Menu, MenuBar, MenuItem, ScrollPane, TextField, TitledPane}
import javafx.scene.layout.{GridPane, StackPane, VBox}

import nutcracker.util.KMap
import org.reactfx.value.Val
import org.reactfx.{EventSource, EventStream}
import protein.capability.Rule
import protein.mechanism.{Protein, Site}
import protein.ui.FactType.{FactPhosTarget, FactRule}
import protein.ui.util.syntax._

import scalaz.Show

class KBWidget {

  private val newFactMenu = new Menu("Add fact") <| { _.getItems.addAll(
    new MenuItem("Bind") <| { _.onAction(() => showDialog(new BindFactInput){ case (p, ps, q, qs) => ReqAssertBind(p, ps, q, qs) }) },
    new MenuItem("Phosphorylatable site") <| { _.onAction(() => showDialog(new PhosSiteFactInput){ case (k, s, ss) => ReqAssertPhosSite(k, s, ss) }) }
  ).ignoreResult }

  private val dialogHolder = new StackPane()
  private val rules = new ListView[Rule]
  private val phosSites = new ListView[(Protein, Protein, Site)]

  val node: Node = new ScrollPane(
    new VBox(
      new Label("Knowledge base") <| { _.setStyle("-fx-font-weight: bold") },
      new MenuBar(newFactMenu),
      dialogHolder,
      new TitledPane("Rules", rules),
      new TitledPane("Phosphorylation target sites", phosSites)
    ) <| {
      _.setFillWidth(true) } <| {
      _.setAlignment(Pos.TOP_CENTER)
    }
  ) <| {
    _.setFitToWidth(true)
  }

  private val _requests = new EventSource[UIRequest]

  private val factHandlers: KMap[FactType, ? => Unit] = KMap[FactType, ? => Unit]()
    .put(FactRule)(ruleAdded)
    .put(FactPhosTarget)({ case (k, s, ss) => phosTargetAdded(k, s, ss) })

  def requests: EventStream[UIRequest] = _requests

  def factAdded[A](t: FactType[A], fact: A)(implicit A: Show[A]): Unit = {
    factHandlers.getOrElse(t)(a => sys.error(s"Unexpected fact type: $t"))(fact)
  }

  private def ruleAdded(r: Rule): Unit = rules.getItems.add(r).ignoreResult()

  private def phosTargetAdded(k: Protein, s: Protein, ss: Site): Unit = phosSites.getItems.add((k, s, ss)).ignoreResult()

  private def showDialog[A](form: InputForm[A])(req: A => UIRequest): Unit =
    InputDialog.show(dialogHolder, form)(a => _requests.push(req(a)))
}

object KBWidget {
  def apply(): KBWidget = new KBWidget
}

trait FactType[A] { self: Singleton => }
object FactType {
  object FactRule extends FactType[Rule]
  object FactPhosTarget extends FactType[(Protein, Protein, Site)]
}

class BindFactInput extends InputForm[(Protein, Site, Protein, Site)] {
  private val p1 = new TextField()
  private val s1 = new TextField()
  private val p2 = new TextField()
  private val s2 = new TextField()

  val title = "Bind"

  val node = new GridPane() <| {
    _.addRow(0, new Label("Protein 1"), p1, new Label("site"), s1) } <| {
    _.addRow(1, new Label("Protein 2"), p2, new Label("site"), s2)
  }

  val input: Val[(Protein, Site, Protein, Site)] = (p1.textProperty() |@| s1.textProperty() |@| p2.textProperty() |@| s2.textProperty()).tuple
    .filter4[String, String, String, String]((p1, s1, p2, s2) => !p1.isEmpty && !s1.isEmpty && !p2.isEmpty && !s2.isEmpty)
    .map4[String, String, String, String, (Protein, Site, Protein, Site)]((p1, s1, p2, s2) => (Protein(p1), Site(s1), Protein(p2), Site(s2)))
}

class PhosSiteFactInput extends InputForm[(Protein, Protein, Site)] {
  private val kinase = new TextField()
  private val substrate = new TextField()
  private val phosSite = new TextField()

  val title = "Phosphorylation target"

  val node = new GridPane() <| { gp =>
    gp.addRow(0, new Label("Kinase"), kinase)
    gp.addRow(1, new Label("Substrate"), substrate, new Label("Target site"), phosSite)
  }

  val input: Val[(Protein, Protein, Site)] = (kinase.textProperty() |@| substrate.textProperty() |@| phosSite.textProperty()).tuple
    .filter3[String, String, String]((k, s, ss) => !k.isEmpty && !s.isEmpty && !ss.isEmpty)
    .map3[String, String, String, (Protein, Protein, Site)]((k, s, ss) => (Protein(k), Protein(s), Site(ss)))
}