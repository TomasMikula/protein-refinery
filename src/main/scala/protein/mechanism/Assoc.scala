package protein.mechanism

import Assoc._

case class Assoc(
  leftEnd: LeftEnd,
  midPoints: Seq[MidPoint],
  rightEnd: RightEnd
) {
  override def toString = leftEnd.toString + " - " + midPoints.map(_.toString + " - ").mkString("") + rightEnd.toString
}

object Assoc {

  final case class LeftEnd(
    p: Protein,
    condition: ProteinModifications,
    toRight: Site
  ) {
    override def toString = s"$p$condition>$toRight"
  }

  final case class RightEnd(
    p: Protein,
    condition: ProteinModifications,
    toLeft: Site
  ) {
    override def toString = s"$toLeft<$p$condition"
  }

  final case class MidPoint(
    p: Protein,
    condition: ProteinModifications,
    toRight: Site,
    toLeft: Site
  ) {
    override def toString = s"$toLeft<$p$condition>$toRight"
  }
}