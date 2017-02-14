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
    import nutcracker.Propagation.module.Ref
    val kbWidget = KBWidget[Ref]()
    val goalWidget = GoalWidget[Ref]()
    val controller = Controller(kbWidget, goalWidget)
    val content = new SplitPane(kbWidget.node, goalWidget.node)

    stage.setTitle("Protein Refinery")
    stage.setScene(new Scene(content))
    stage.show()
  }

}
