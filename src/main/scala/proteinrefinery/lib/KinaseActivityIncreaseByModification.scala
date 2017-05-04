package proteinrefinery.lib

case class KinaseActivityIncreaseByModification[Ref[_]](kinase: Protein, mods: ProteinModifications[Ref])
