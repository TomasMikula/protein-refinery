package protein.ui

import javafx.scene.Node
import org.reactfx.value.Val

abstract class InputForm[A] {
  def title: String
  def node: Node
  def input: Val[A]
}
