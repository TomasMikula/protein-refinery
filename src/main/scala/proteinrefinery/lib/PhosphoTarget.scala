package proteinrefinery.lib

import nutcracker.Antichain

final case class PhosphoTarget(kinase: Protein, substrate: Protein, targetSite: Site) {

}

object PhosphoTarget {
  type Ref = Antichain.Ref[PhosphoTarget]
}