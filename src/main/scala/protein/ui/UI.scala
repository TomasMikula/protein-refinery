package protein.ui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.control.SplitPane
import javafx.stage.Stage

object UI extends App {
  Application.launch(classOf[UIApp])
}

class UIApp extends Application {

  def start(stage: Stage): Unit = {
    val kbWidget = KBWidget()
    val goalWidget = GoalWidget()
    val controller = Controller(kbWidget, goalWidget)
    val content = new SplitPane(kbWidget.node, goalWidget.node)

    stage.setScene(new Scene(content))
    stage.show()
  }

}
