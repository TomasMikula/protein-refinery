package proteinrefinery.util

/** Allows to ask for the set of morphisms between two objects. */
trait HomSet[D] {
  type HomSet

  def homSet(d1: D, d2: D): HomSet
}

object HomSet {
  type Aux[D, HS] = HomSet[D] { type HomSet = HS }

  sealed trait TerminalOr[+H]
  case object Terminal extends TerminalOr[Nothing]
  case class Morphisms[H](hs: List[H]) extends TerminalOr[H]
}