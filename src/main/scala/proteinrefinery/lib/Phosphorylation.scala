package proteinrefinery.lib

import nutcracker.{Discrete, IncSet, Trigger}
import nutcracker.ops._
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

  trait Search[M[_], Var[_], Val[_]] {
    protected implicit def Propagation: nutcracker.Propagation[M, Var, Val]
    implicit def Tracking: proteinrefinery.util.Tracking[M, Var, Val]

    def IncSets: nutcracker.IncSets[M, Var, Val]
    def Nuggets: proteinrefinery.lib.Nuggets[M, Var, Val]
    def AssocSearch: Assoc.Search[M, Var, Val]

    def phosphorylationsC(kinase: Protein, substrate: Protein)(implicit M: Monad[M]): ContU[M, PhosphoTarget.Ref[Var]] =
      ContU(f =>
        Nuggets.phosphoTargets(ptRef => ptRef.observe.by_(apt => {
          val pt = apt.value
          if (pt.kinase === kinase && pt.substrate === substrate) Trigger.fireReload(f(ptRef).as(Trigger.sleep((d, δ) => ???)))
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