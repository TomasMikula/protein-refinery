package protein

import nutcracker.BranchLang._
import nutcracker.Domain
import nutcracker.Domain.Values
import nutcracker.util.free.{InjectK, FreeK}
import protein.mechanism.ProteinModifications

import scalaz.Id._
import scalaz.StreamT

package object search {

  type ProteinModificationsLattice = Option[ProteinModifications]

  implicit def proteinModificationsLattice: Domain[ProteinModifications, ProteinModificationsLattice] =
    new Domain[ProteinModifications, ProteinModificationsLattice] {
      def values(d: ProteinModificationsLattice): Values[ProteinModifications, ProteinModificationsLattice] = d match {
        case None => Domain.Empty()
        case Some(a) => Domain.Just(a)
      }

      def sizeUpperBound(d: ProteinModificationsLattice): Option[Long] = d match {
        case None => Some(0L)
        case Some(a) => Some(1L)
      }

      def singleton(a: ProteinModifications): ProteinModificationsLattice = Some(a)

      def refine(x: ProteinModificationsLattice, y: ProteinModificationsLattice): Option[ProteinModificationsLattice] = {
        val res = meet(x, y)
        if (res == x) None else Some(res)
      }

      def meet(x: ProteinModificationsLattice, y: ProteinModificationsLattice): ProteinModificationsLattice =
        (x, y) match {
          case (Some(p), Some(q)) => p combine q
          case _ => None
        }
    }

  def branch(ks: FreeK[Vocabulary, Unit]*): FreeK[Vocabulary, Unit] = {
    implicit val inj = implicitly[InjectK[protein.SearchLang.BranchL, Vocabulary]]
    addBranchingF[StreamT[Id, ?], Vocabulary](StreamT.fromIterable(ks))
  }
}
