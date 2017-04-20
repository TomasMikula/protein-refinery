package proteinrefinery.ui

import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.control.{Button, Label, ListView, Menu, MenuBar, MenuItem, ScrollPane, TextField, TitledPane}
import javafx.scene.layout.{GridPane, HBox, StackPane, VBox}

import nutcracker.util.HHKMap
import org.reactfx.collection.LiveArrayList
import org.reactfx.value.{Val, Var}
import org.reactfx.{EventSource, EventStream, EventStreams}
import proteinrefinery.lib.{ISite, PhosphoTarget, Protein, ProteinModifications, ProteinPattern, Rule, SiteLabel, SiteState}
import proteinrefinery.ui.FactType.{FactKinase, FactPhosTarget, FactRule}
import proteinrefinery.ui.util.syntax._

import scalaz.Show

class KBWidget[Ref[_]] {

  private val newFactMenu = new Menu("Add fact") <| { _.getItems.addAll(
    new MenuItem("Bind") <| { _.onAction(() => showDialog(new BindFactInput){ case (p, ps, q, qs) => ReqAssertBind(p, ps, q, qs) }) },
    new MenuItem("Kinase activity") <| { _.onAction(() => showDialog(new KinaseActivityInput[Ref])(pp => ReqAssertKinaseActivity(pp))) },
    new MenuItem("Phosphorylatable site") <| { _.onAction(() => showDialog(new PhosSiteFactInput){ case (k, s, ss) => ReqAssertPhosSite(k, s, ss) }) }
  ).ignoreResult }

  private val dialogHolder = new StackPane()
  private val rules = new ListView[Rule[Ref]]
  private val phosSites = new ListView[(Protein, Protein, ISite[Ref])]
  private val kinases = new ListView[ProteinPattern[Ref]]

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

  private val _requests = new EventSource[UIRequest[Ref]]

  private val factHandlers: HHKMap[FactType, λ[`A[_[_]]` => A[Ref] => Unit]] =
    HHKMap[FactType, λ[`A[_[_]]` => A[Ref] => Unit]]()
      .put(FactRule)(ruleAdded)
      .put(FactPhosTarget)(pt => phosTargetAdded(pt.kinase.protein, pt.substrate.protein, pt.targetSite))
      .put(FactKinase)(kinaseAdded)

  def requests: EventStream[UIRequest[Ref]] = _requests

  def factAdded[A[_[_]]](t: FactType[A], fact: A[Ref])(implicit A: Show[A[Ref]]): Unit = {
    factHandlers.getOrElse(t)(a => sys.error(s"Unexpected fact type: $t"))(fact)
  }

  private def ruleAdded(r: Rule[Ref]): Unit = rules.getItems.add(r).ignoreResult()

  private def phosTargetAdded(k: Protein, s: Protein, ss: ISite[Ref]): Unit = phosSites.getItems.add((k, s, ss)).ignoreResult()

  private def kinaseAdded(pp: ProteinPattern[Ref]): Unit = kinases.getItems.add(pp).ignoreResult()

  private def showDialog[A](form: InputForm[A])(req: A => UIRequest[Ref]): Unit =
    InputDialog.show(dialogHolder, form)(a => _requests.push(req(a)))
}

object KBWidget {
  def apply[Ref[_]](): KBWidget[Ref] = new KBWidget
}

trait FactType[A[_[_]]] { self: Singleton =>
  type Data[Ref[_]] = A[Ref]
}
object FactType {
  object FactRule extends FactType[Rule]
  object FactKinase extends FactType[ProteinPattern]
  object FactPhosTarget extends FactType[PhosphoTarget]
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

class KinaseActivityInput[Ref[_]] extends InputForm[ProteinPattern[Ref]] {
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
    new Button("+") <| { _.onAction(addSiteRow _) }
  )

  val input: Val[ProteinPattern[Ref]] = {
    val p = protein.textProperty().map1(s => if(s.nonEmpty) Protein(s) else null)
    val mods: Var[ProteinModifications[Ref]] = Var.newSimpleVar(ProteinModifications.noModifications)
    EventStreams.merge(siteFields, j((tf: TextField) => EventStreams.valuesOf(tf.textProperty()))).forEach(mod => {
      var m: ProteinModifications[Ref] = ProteinModifications.noModifications
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