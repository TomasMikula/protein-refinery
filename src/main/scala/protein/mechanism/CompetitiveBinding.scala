package protein.mechanism

case class CompetitiveBinding(
  base: Binding,
  competing: Binding
) {
  assert(base.rightPattern overlaps competing.rightPattern)

  override def toString = s"Binding ${competing} competes with binding ${base}"
}
