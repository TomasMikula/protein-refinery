package proteinrefinery.lib

import scala.language.higherKinds
//import nutcracker.Discrete
import nutcracker.util.{ContU, EqualK}

import scalaz.Monad

sealed trait PositiveInfluenceOnPhosphorylation[Ref[_]] {
  def agent: ProteinPattern[Ref]
  def phosphorylation: Phosphorylation[Ref]
}

//object PositiveInfluenceOnPhosphorylation {
//
//  type Ref[Var[_]] = Var[Discrete[PositiveInfluenceOnPhosphorylation[Var]]]
//
//  // Constructors
//
//  /** Agent that acts as the kinase in a phosphorylation
//    * is considered to have positive influence on that phosphorylation.
//    */
//  final case class IsKinase[Var[_]](phosphorylation: Phosphorylation[Var]) extends PositiveInfluenceOnPhosphorylation[Var] {
//    def agent = ProteinPattern(phosphorylation.kinase)
//  }
//
//  /** Agent that is part of the scaffold connecting kinase to substrate
//    * is considered to have positive influence on the phosphorylation.
//    */
//  final case class InScaffold[Var[_]](agent: ProteinPattern[Var], phosphorylation: Phosphorylation[Var]) extends PositiveInfluenceOnPhosphorylation[Var]
//
//  /** Agent that has positive influence on kinase activity of the kinase involved in phosphorylation
//    * is considered to have positive influence on that phosphorylation.
//    */
//  final case class ActivatesKinase[Var[_]](activation: PositiveInfluenceOnKinaseActivity[Var], phosphorylation: Phosphorylation[Var]) extends PositiveInfluenceOnPhosphorylation[Var] {
//    def agent = activation.agent
//  }
//
//
////  trait Search[M[_], Var[_]] { self: PositiveInfluenceOnKinaseActivity.Search[M, Var] =>
////
////    implicit def Propagation: nutcracker.Propagation[M, Var]
////
////    def positiveInfluenceOnPhosphorylationC(p: Protein, ph: Phosphorylation[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
////
////      // we can immediately tell whether `p` is the kinase or part of the scaffold in `ph`
////      val immediate: List[ContU[M, Ref[Var]]] = {
////        val isKinase = if (ph.kinase == p) List(IsKinase(ph)) else Nil
////        val inScaffold = if (ph.assoc.bindings.tail.exists(_.left == p)) List(InScaffold(ProteinPattern(p), ph)) else Nil
////        (isKinase ++ inScaffold).map(Discrete.cellC[M, Var, PositiveInfluenceOnPhosphorylation[Var]](_))
////      }
////
////      val indirect: ContU[M, Ref[Var]] = Discrete.map(positiveInfluenceOnKinaseActivityC(p, ph.kinase))(infl => ActivatesKinase(infl, ph))
////
////      ContU.sequence(indirect :: immediate)
////    }
////
////  }
//
//}

trait PositiveInfluenceOnPhosphorylatedStateSearch[M[_], Ref[_], Val[_]] { self: PositiveInfluenceOnState.Search[M, Ref, Val] =>

  def Nuggets: proteinrefinery.lib.Nuggets[M, Ref, Val]

  def positiveInfluenceOnPhosphorylatedStateC(agent: Protein, target: Protein)(implicit M: Monad[M], E: EqualK[Ref]): ContU[M, PositiveInfluenceOnState.Ref[Ref]] =
    Nuggets.phosphoSitesC(target).flatMap(site => {
      val pat = ProteinPattern[Ref](target).addModification(site, SiteState("p")) // XXX hard-coded phosphorylated state as "p"
      positiveInfluenceOnStateC(agent, pat)
    })

}