package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Discrete, IncSet, Trigger}
import nutcracker.util.ContU

import scalaz.{Monad, Show}
import scalaz.syntax.equal._
import scalaz.syntax.monad._

case class Phosphorylation[Ref[_]](
  assoc: Assoc[Ref],
  phosphoSite: SiteLabel
) {
//  def kinase: Protein = assoc.bindings.head.left

  override def toString = s"${assoc.bindings.head.left} phosphorylates ${assoc.bindings.last.right} at $phosphoSite (via $assoc)"
}

object Phosphorylation {

  type Ref[Var[_]] = Var[Discrete[Phosphorylation[Var]]]

  trait Search[M[_], Var[_]] {
    protected implicit def Propagation: nutcracker.Propagation[M, Var]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var]

    def IncSets: nutcracker.IncSets[M, Var]
    def Nuggets: proteinrefinery.lib.Nuggets[M, Var]
    def AssocSearch: Assoc.Search[M, Var]

    def phosphorylationsC(kinase: Protein, substrate: Protein)(implicit M: Monad[M]): ContU[M, PhosphoTarget.Ref[Var]] =
      ContU(f =>
        Nuggets.phosphoTargets(ptRef => Propagation.observe(ptRef).by(apt => {
          val pt = apt.value
          if (pt.kinase === kinase && pt.substrate === substrate) Trigger.fireReload(f(ptRef) map (_ => (d, δ) => ???))
          else Trigger.sleep((d, δ) => ???)
        }))
      )

    def phosphorylations(kinase: Protein, substrate: Protein)(implicit M: Monad[M]): M[Var[IncSet[PhosphoTarget.Ref[Var]]]] =
      IncSets.collect(phosphorylationsC(kinase, substrate))

  }


  // Typeclass instances

  implicit def showInstance[Var[_]]: Show[Phosphorylation[Var]] = new Show[Phosphorylation[Var]] {
    override def shows(p: Phosphorylation[Var]): String = p.toString
  }
}