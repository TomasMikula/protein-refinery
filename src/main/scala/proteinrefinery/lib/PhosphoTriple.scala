package proteinrefinery.lib

import scala.language.higherKinds

case class PhosphoTriple[Var[_]](kinase: Protein, substrate: Protein, targetSite: ISite[Var]) {

}
