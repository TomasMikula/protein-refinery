package protein.mechanism

case class Assoc(bindings: List[Binding]) extends AnyVal {
  override def toString = bindings.mkString(" ; ")
}