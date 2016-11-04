package proteinrefinery.lib

import scala.language.higherKinds

case class KinaseActivityIncreaseByModification[Ref[_]](kinase: Protein, mods: ProteinModifications[Ref])
