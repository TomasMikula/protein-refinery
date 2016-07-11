package protein

import nutcracker.{Dom, Meet}

package object lib {

  type ProteinModificationsLattice = Option[ProteinModifications]

  implicit def proteinModificationsLattice: Dom.Aux[ProteinModificationsLattice, Meet[ProteinModificationsLattice], Unit] =
    new Dom[ProteinModificationsLattice] {
      type Update = Meet[ProteinModificationsLattice]
      type Delta = Unit
      override def assess(d: ProteinModificationsLattice): Dom.Status[Meet[ProteinModificationsLattice]] = d match {
        case None => Dom.Failed
        case Some(a) => Dom.Refined
      }

      override def update(d: ProteinModificationsLattice, u: Meet[ProteinModificationsLattice]): Option[(ProteinModificationsLattice, Unit)] = {
        val res = (d, u.value) match {
          case (Some(p), Some(q)) => p combine q
          case _ => None
        }
        if(res == d) None else Some((res, ()))
      }

      override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
    }

}
