package proteinrefinery

import nutcracker.{Dom, Join}

package object lib {

  type ProteinModificationsLattice = Option[ProteinModifications]

  implicit def proteinModificationsLattice: Dom.Aux[ProteinModificationsLattice, Join[ProteinModificationsLattice], Unit] =
    new Dom[ProteinModificationsLattice] {
      type Update = Join[ProteinModificationsLattice]
      type Delta = Unit
      override def assess(d: ProteinModificationsLattice): Dom.Status[Join[ProteinModificationsLattice]] = d match {
        case None => Dom.Failed
        case Some(a) => Dom.Refined
      }

      override def update(d: ProteinModificationsLattice, u: Join[ProteinModificationsLattice]): Option[(ProteinModificationsLattice, Unit)] = {
        val res = (d, u.value) match {
          case (Some(p), Some(q)) => p combine q
          case _ => None
        }
        if(res == d) None else Some((res, ()))
      }

      override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
    }

}
