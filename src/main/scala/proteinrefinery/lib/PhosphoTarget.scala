package proteinrefinery.lib

import scala.language.higherKinds

import nutcracker.Antichain

final case class PhosphoTarget(kinase: Protein, substrate: Protein, targetSite: SiteLabel) {

}

object PhosphoTarget {
  type Ref[Var[_]] = Var[Antichain[PhosphoTarget]]
}