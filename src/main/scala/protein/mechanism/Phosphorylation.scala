package protein.mechanism

case class Phosphorylation(
  assoc: Assoc,
  phosphoSite: Site
) {
  override def toString = s"${assoc.bindings.head.left} phosphorylates ${assoc.bindings.last.right} at $phosphoSite (via $assoc)"
}
