package proteinrefinery.lib

import proteinrefinery.util.HomSet

case class SiteState(label: String) {
  override def toString = label
}

object SiteState {
  implicit def homSet: HomSet.Aux[SiteState, List[Unit]] = new HomSet[SiteState] {
    type HomSet = List[Unit]

    def homSet(s1: SiteState, s2: SiteState): HomSet =
      if (s1 == s2) List(())
      else Nil
  }
}