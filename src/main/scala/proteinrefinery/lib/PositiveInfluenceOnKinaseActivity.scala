package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Antichain
import nutcracker.util.{ContU, EqualK}

import scalaz.Monad

sealed trait PositiveInfluenceOnKinaseActivity[Ref[_]] {
  def agent: ProteinPattern[Ref]
  def kinase: Protein
}

object PositiveInfluenceOnKinaseActivity {

  type Ref[Var[_]] = Var[Antichain[PositiveInfluenceOnKinaseActivity[Var]]]

  // Constructors

  final case class PositiveInfluenceOnActiveState[Var[_]](infl: PositiveInfluenceOnState[Var]) extends PositiveInfluenceOnKinaseActivity[Var] {
    def agent = infl.agent
    def kinase: Protein = infl.target.protein
  }
  def positiveInfluenceOnActiveState[Var[_]](infl: PositiveInfluenceOnState[Var]): PositiveInfluenceOnKinaseActivity[Var] =
    PositiveInfluenceOnActiveState(infl)


  trait Search[M[_], Var[_]] { self: PositiveInfluenceOnState.Search[M, Var] =>

    def Nuggets: proteinrefinery.lib.Nuggets[M, Var]

    def positiveInfluenceOnKinaseActivityC(agent: Protein, kinase: Protein)(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = for {
      ppref <- Nuggets.kinaseActivityC(kinase)
      res <- Antichain.map(Antichain.mapC(ppref)(pp => positiveInfluenceOnStateC(agent, pp)))(positiveInfluenceOnActiveState(_))
    } yield res

  }

}