package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Antichain
import nutcracker.util.ContU

import scalaz.Monad

sealed trait PositiveInfluenceOnPhosphorylation {
  def agent: ProteinPattern
  def phosphorylation: Phosphorylation
}

object PositiveInfluenceOnPhosphorylation {

  type Ref = Antichain.Ref[PositiveInfluenceOnPhosphorylation]

  // Constructors

  /** Agent that acts as the kinase in a phosphorylation
    * is considered to have positive influence on that phosphorylation.
    */
  final case class IsKinase(phosphorylation: Phosphorylation) extends PositiveInfluenceOnPhosphorylation {
    def agent = ProteinPattern(phosphorylation.kinase)
  }

  /** Agent that is part of the scaffold connecting kinase to substrate
    * is considered to have positive influence on the phosphorylation.
    */
  final case class InScaffold(agent: ProteinPattern, phosphorylation: Phosphorylation) extends PositiveInfluenceOnPhosphorylation

  /** Agent that has positive influence on kinase activity of the kinase involved in phosphorylation
    * is considered to have positive influence on that phosphorylation.
    */
  final case class ActivatesKinase(activation: PositiveInfluenceOnKinaseActivity, phosphorylation: Phosphorylation) extends PositiveInfluenceOnPhosphorylation {
    def agent = activation.agent
  }


  trait Search[M[_]] { self: PositiveInfluenceOnKinaseActivity.Search[M] =>

    implicit def Propagation: nutcracker.Propagation[M]

    def positiveInfluenceOnPhosphorylationC(p: Protein, ph: Phosphorylation)(implicit M: Monad[M]): ContU[M, Ref] = {

      // we can immediately tell whether `p` is the kinase or part of the scaffold in `ph`
      val immediate: List[ContU[M, Ref]] = {
        val isKinase = if (ph.kinase == p) List(IsKinase(ph)) else Nil
        val inScaffold = if (ph.assoc.bindings.tail.exists(_.left == p)) List(InScaffold(ProteinPattern(p), ph)) else Nil
        (isKinase ++ inScaffold).map(Antichain.cellC[M, PositiveInfluenceOnPhosphorylation](_))
      }

      val indirect: ContU[M, Ref] = Antichain.map(positiveInfluenceOnKinaseActivityC(p, ph.kinase))(infl => ActivatesKinase(infl, ph))

      ContU.sequence(indirect :: immediate)
    }

  }

}

trait PositiveInfluenceOnPhosphorylatedStateSearch[M[_]] { self: PositiveInfluenceOnState.Search[M] =>

  def Nuggets: proteinrefinery.lib.Nuggets[M]

  def positiveInfluenceOnPhosphorylatedStateC(agent: Protein, target: Protein)(implicit M: Monad[M]): ContU[M, PositiveInfluenceOnState.Ref] =
    Nuggets.phosphoSitesC(target).flatMap(site => {
      val pat = ProteinPattern(target).addModification(site, SiteState("p")) // XXX hard-coded phosphorylated state as "p"
      positiveInfluenceOnStateC(agent, pat)
    })

}