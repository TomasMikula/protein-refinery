package protein.search

import nutcracker.DomRef
import nutcracker.Promised
import protein.capability.Binding
import protein.mechanism.{Site, Protein}

import Assoc._

case class Assoc(
  leftEnd: LeftEnd,
  rightEnd: RightEnd
)

object Assoc {

  sealed trait Elem {
    def protein: Protein
  }

  sealed trait RightConnected extends Elem {
    def toRight: DomRef[Site, Set[Site]]
    def bindingToRight: Promised[Binding]
    def right: Promised[LeftConnected]
  }

  sealed trait LeftConnected extends Elem {
    def toLeft: DomRef[Site, Set[Site]]
    def left: Promised[RightConnected]
  }

  final case class LeftEnd(
    protein: Protein,
    toRight: DomRef[Site, Set[Site]],
    bindingToRight: Promised[Binding],
    right: Promised[LeftConnected]
  ) extends RightConnected

  final case class MidPoint(
    protein: Protein,
    toRight: DomRef[Site, Set[Site]],
    bindingToRight: Promised[Binding],
    right: Promised[LeftConnected],
    toLeft: DomRef[Site, Set[Site]],
    left: Promised[RightConnected]
  ) extends RightConnected with LeftConnected

  final case class RightEnd(
    protein: Protein,
    toLeft: DomRef[Site, Set[Site]],
    left: Promised[RightConnected]
  ) extends LeftConnected

}