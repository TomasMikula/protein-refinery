package proteinrefinery.ui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.control.SplitPane
import javafx.stage.Stage

object UI extends App {
  Application.launch(classOf[UIApp])
}

class UIApp extends Application {

  def start(stage: Stage): Unit = {
    import Controller.Var
    val kbWidget = KBWidget[Var]()
    val goalWidget = GoalWidget[Var]()
    val controller = Controller(kbWidget, goalWidget)
    val content = new SplitPane(kbWidget.node, goalWidget.node)

    stage.setTitle("Protein Refinery")
    stage.setScene(new Scene(content))
    stage.show()
  }

}
