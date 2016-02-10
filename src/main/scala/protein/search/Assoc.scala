package protein.search

import nutcracker.DomRef
import nutcracker.PromiseLang._
import nutcracker.PropagationLang._
import nutcracker.util.free.FreeK
import protein.capability.Binding
import protein.{KB, Vocabulary}
import protein.mechanism.{CompetitiveBinding, ProteinModifications, Site, Protein}
import protein.search.Assoc._

case class Assoc(
  leftEnd: LeftEnd,
  rightEnd: RightEnd
)

object Assoc {

  sealed trait Elem {
    def protein: DomRef[Protein, Set[Protein]]
    def condition: DomRef[ProteinModifications, ProteinModificationsLattice]
  }

  sealed trait RightConnected extends Elem {
    def toRight: DomRef[Site, Set[Site]]
    def right: Promised[LeftConnected]
  }

  sealed trait LeftConnected extends Elem {
    def toLeft: DomRef[Site, Set[Site]]
    def left: Promised[RightConnected]
  }

  final case class LeftEnd(
    protein: DomRef[Protein, Set[Protein]],
    condition: DomRef[ProteinModifications, ProteinModificationsLattice],
    toRight: DomRef[Site, Set[Site]],
    right: Promised[LeftConnected]
  ) extends RightConnected

  final case class MidPoint(
    protein: DomRef[Protein, Set[Protein]],
    condition: DomRef[ProteinModifications, ProteinModificationsLattice],
    toRight: DomRef[Site, Set[Site]],
    right: Promised[LeftConnected],
    toLeft: DomRef[Site, Set[Site]],
    left: Promised[RightConnected]
  ) extends RightConnected with LeftConnected

  final case class RightEnd(
    protein: DomRef[Protein, Set[Protein]],
    condition: DomRef[ProteinModifications, ProteinModificationsLattice],
    toLeft: DomRef[Site, Set[Site]],
    left: Promised[RightConnected]
  ) extends LeftConnected

}