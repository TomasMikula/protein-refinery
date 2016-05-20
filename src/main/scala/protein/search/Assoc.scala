package protein.search

import nutcracker.DecSet.DecSetRef
import nutcracker.Promised
import protein.mechanism.{Binding, Protein, Site}
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
    def toRight: DecSetRef[Site]
    def bindingToRight: Promised[Binding]
    def right: Promised[LeftConnected]
  }

  sealed trait LeftConnected extends Elem {
    def toLeft: DecSetRef[Site]
    def left: Promised[RightConnected]
  }

  final case class LeftEnd(
    protein: Protein,
    toRight: DecSetRef[Site],
    bindingToRight: Promised[Binding],
    right: Promised[LeftConnected]
  ) extends RightConnected

  final case class MidPoint(
    protein: Protein,
    toRight: DecSetRef[Site],
    bindingToRight: Promised[Binding],
    right: Promised[LeftConnected],
    toLeft: DecSetRef[Site],
    left: Promised[RightConnected]
  ) extends RightConnected with LeftConnected

  final case class RightEnd(
    protein: Protein,
    toLeft: DecSetRef[Site],
    left: Promised[RightConnected]
  ) extends LeftConnected

}