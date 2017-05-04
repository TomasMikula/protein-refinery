package proteinrefinery.lib

case class PhosphoTriple[Var[_]](kinase: Protein, substrate: Protein, targetSite: ISite[Var]) {

}
