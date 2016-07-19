package proteinrefinery.ui

import java.lang.Boolean.{FALSE, TRUE}
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{HBox, Pane, VBox}

import proteinrefinery.ui.util.syntax._

class InputDialog[A](form: InputForm[A])(submit: A => Unit, cancel: () => Unit) { dialog =>
  val node = new VBox(
    new Label(form.title) <| { _.setStyle("-fx-font-weight: bold") },
    form.node,
    new HBox(
      new Button("Cancel")
        <| { _.onAction(cancel) },
      new Button("OK")
        <| { _.onAction(() => submit(form.input.getValue)) }
        <| { _.disableProperty().bind(form.input.map1(_ => FALSE).orElseConst(TRUE)) }
    ) <| {
      _.setAlignment(Pos.CENTER_RIGHT)
    }
  ) <| {
    _.setFillWidth(true)
  }
}

object InputDialog {
  def show[A](dialogHolder: Pane, form: InputForm[A])(onSubmit: A => Unit): Unit = {
    def hideDialog(): Unit = dialogHolder.getChildren.clear()
    hideDialog()
    dialogHolder.getChildren.add(
      new InputDialog(form)(a => { hideDialog(); onSubmit(a) }, hideDialog).node
    ).ignoreResult
  }
}