package protein.mechanism

case class Phosphorylation(
  assoc: Assoc,
  phosphoSite: Site
) {
  override def toString = s"${assoc.bindings.head.left.p.p} phosphorylates ${assoc.bindings.last.right.p.p} at $phosphoSite (via $assoc)"
}
