package protein.mechanism

case class Phosphorylation(
  assoc: Assoc,
  phosphoSite: Site
) {
  override def toString = s"${assoc.leftEnd.p} phosphorylates ${assoc.rightEnd.p} at $phosphoSite (via $assoc)"
}
