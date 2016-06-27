package protein.mechanism

import scalaz.Show

case class Assoc(bindings: List[Binding]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}

object Assoc {
  implicit def showInstance: Show[Assoc] = new Show[Assoc] {
    override def shows(a: Assoc): String = a.toString
  }
}