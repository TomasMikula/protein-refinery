package proteinrefinery.ui

import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.control.{Button, Label, ListView, Menu, MenuBar, MenuItem, ScrollPane, TextField, TitledPane}
import javafx.scene.layout.{GridPane, HBox, StackPane, VBox}

import nutcracker.util.KMap
import org.reactfx.collection.LiveArrayList
import org.reactfx.value.{Val, Var}
import org.reactfx.{EventSource, EventStream, EventStreams}
import proteinrefinery.lib.{Protein, ProteinModifications, ProteinPattern, Rule, SiteLabel, SiteState}
import proteinrefinery.ui.FactType.{FactKinase, FactPhosTarget, FactRule}
import proteinrefinery.ui.util.syntax._

import scalaz.Show

class KBWidget {

  private val newFactMenu = new Menu("Add fact") <| { _.getItems.addAll(
    new MenuItem("Bind") <| { _.onAction(() => showDialog(new BindFactInput){ case (p, ps, q, qs) => ReqAssertBind(p, ps, q, qs) }) },
    new MenuItem("Kinase activity") <| { _.onAction(() => showDialog(new KinaseActivityInput)(pp => ReqAssertKinaseActivity(pp))) },
    new MenuItem("Phosphorylatable site") <| { _.onAction(() => showDialog(new PhosSiteFactInput){ case (k, s, ss) => ReqAssertPhosSite(k, s, ss) }) }
  ).ignoreResult }

  private val dialogHolder = new StackPane()
  private val rules = new ListView[Rule]
  private val phosSites = new ListView[(Protein, Protein, SiteLabel)]
  private val kinases = new ListView[ProteinPattern]

  val node: Node = new ScrollPane(
    new VBox(
      new Label("Knowledge base") <| { _.setStyle("-fx-font-weight: bold") },
      new MenuBar(newFactMenu),
      dialogHolder,
      new TitledPane("Rules", rules),
      new TitledPane("Kinase activity", kinases),
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
    .put(FactKinase)(kinaseAdded)

  def requests: EventStream[UIRequest] = _requests

  def factAdded[A](t: FactType[A], fact: A)(implicit A: Show[A]): Unit = {
    factHandlers.getOrElse(t)(a => sys.error(s"Unexpected fact type: $t"))(fact)
  }

  private def ruleAdded(r: Rule): Unit = rules.getItems.add(r).ignoreResult()

  private def phosTargetAdded(k: Protein, s: Protein, ss: SiteLabel): Unit = phosSites.getItems.add((k, s, ss)).ignoreResult()

  private def kinaseAdded(pp: ProteinPattern): Unit = kinases.getItems.add(pp).ignoreResult()

  private def showDialog[A](form: InputForm[A])(req: A => UIRequest): Unit =
    InputDialog.show(dialogHolder, form)(a => _requests.push(req(a)))
}

object KBWidget {
  def apply(): KBWidget = new KBWidget
}

trait FactType[A] { self: Singleton => }
object FactType {
  object FactRule extends FactType[Rule]
  object FactKinase extends FactType[ProteinPattern]
  object FactPhosTarget extends FactType[(Protein, Protein, SiteLabel)]
}

class BindFactInput extends InputForm[(Protein, SiteLabel, Protein, SiteLabel)] {
  private val p1 = new TextField()
  private val s1 = new TextField()
  private val p2 = new TextField()
  private val s2 = new TextField()

  val title = "Bind"

  val node = new GridPane() <| {
    _.addRow(0, new Label("Protein 1"), p1, new Label("site"), s1) } <| {
    _.addRow(1, new Label("Protein 2"), p2, new Label("site"), s2)
  }

  val input: Val[(Protein, SiteLabel, Protein, SiteLabel)] = (p1.textProperty() |@| s1.textProperty() |@| p2.textProperty() |@| s2.textProperty()).tuple
    .filter4[String, String, String, String]((p1, s1, p2, s2) => !p1.isEmpty && !s1.isEmpty && !p2.isEmpty && !s2.isEmpty)
    .map4[String, String, String, String, (Protein, SiteLabel, Protein, SiteLabel)]((p1, s1, p2, s2) => (Protein(p1), SiteLabel(s1), Protein(p2), SiteLabel(s2)))
}

class PhosSiteFactInput extends InputForm[(Protein, Protein, SiteLabel)] {
  private val kinase = new TextField()
  private val substrate = new TextField()
  private val phosSite = new TextField()

  val title = "Phosphorylation target"

  val node = new GridPane() <| { gp =>
    gp.addRow(0, new Label("Kinase"), kinase)
    gp.addRow(1, new Label("Substrate"), substrate, new Label("Target site"), phosSite)
  }

  val input: Val[(Protein, Protein, SiteLabel)] = (kinase.textProperty() |@| substrate.textProperty() |@| phosSite.textProperty()).tuple
    .filter3[String, String, String]((k, s, ss) => !k.isEmpty && !s.isEmpty && !ss.isEmpty)
    .map3[String, String, String, (Protein, Protein, SiteLabel)]((k, s, ss) => (Protein(k), Protein(s), SiteLabel(ss)))
}

class KinaseActivityInput extends InputForm[ProteinPattern] {
  private val protein = new TextField()
  private val siteStates = new LiveArrayList[(TextField, TextField)]
  private val siteGrid = new GridPane() <| {
    _.addRow(0, new Label("Site"), new Label("State"))
  }
  private val siteFields = FXCollections.observableSet[TextField]()

  val title = "Kinase activity"

  val node = new VBox(
    new HBox(new Label("Protein"), protein),
    new Label("State conditions:"),
    siteGrid,
    new Button("+") <| { _.onAction(addSiteRow) }
  )

  val input: Val[ProteinPattern] = {
    val p = protein.textProperty().map1(s => if(s.nonEmpty) Protein(s) else null)
    val mods: Var[ProteinModifications] = Var.newSimpleVar(ProteinModifications.noModifications)
    EventStreams.merge(siteFields, j((tf: TextField) => EventStreams.valuesOf(tf.textProperty()))).forEach(mod => {
      var m: ProteinModifications = ProteinModifications.noModifications
      val it = siteStates.iterator()
      while(it.hasNext) {
        val (siteField, stateField)  = it.next()
        val (site, state) = (siteField.getText, stateField.getText)
        m =
          if (site.nonEmpty && state.nonEmpty) m.addModification(SiteLabel(site), SiteState(state))
          else m
      }
      mods.setValue(m)
    })
    (p |@| mods)((p, m) => m.ifAdmissible.map(ProteinPattern(p, _)).getOrElse(null))
  }

  addSiteRow()

  private def addSiteRow(): Unit = {
    val site = new TextField()
    val state = new TextField()
    siteStates.add((site, state))
    siteFields.add(site)
    siteFields.add(state)
    siteGrid.addRow(siteStates.size, site, state)
  }
}