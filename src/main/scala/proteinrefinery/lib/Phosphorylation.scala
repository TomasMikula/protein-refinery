package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Antichain, IncSet}
import nutcracker.util.ContU

import scalaz.{Monad, Show}
import scalaz.syntax.equal._

case class Phosphorylation[Ref[_]](
  assoc: Assoc[Ref],
  phosphoSite: SiteLabel
) {
  def kinase: Protein = assoc.bindings.head.left

  override def toString = s"${assoc.bindings.head.left} phosphorylates ${assoc.bindings.last.right} at $phosphoSite (via $assoc)"
}

object Phosphorylation {

  type Ref[Var[_]] = Var[Antichain[Phosphorylation[Var]]]

  trait Search[M[_], Var[_]] {
    implicit def Propagation: nutcracker.Propagation[M, Var]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var]

    def IncSets: nutcracker.IncSets[M, Var]
    def Nuggets: proteinrefinery.lib.Nuggets[M, Var]
    def AssocSearch: Assoc.Search[M, Var]

    def phosphorylationsC(kinase: Protein, substrate: Protein)(implicit M: Monad[M]): ContU[M, PhosphoTarget.Ref[Var]] =
      ContU(f =>
        Nuggets.phosphoTargetsF(ptRef => Propagation.observe(ptRef).by(apt => {
          val pt = apt.value
          if (pt.kinase === kinase && pt.substrate === substrate) (Some(f(ptRef)), Some((d, δ) => ???))
          else (None, Some((d, δ) => ???))
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