package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Antichain
import nutcracker.util.ContU

import scalaz.Monad

sealed trait PositiveInfluenceOnKinaseActivity {
  def agent: ProteinPattern
  def kinase: Protein
}

object PositiveInfluenceOnKinaseActivity {

  type Ref = Antichain.Ref[PositiveInfluenceOnKinaseActivity]

  // Constructors

  final case class PositiveInfluenceOnActiveState(infl: PositiveInfluenceOnState) extends PositiveInfluenceOnKinaseActivity {
    def agent = infl.agent
    def kinase: Protein = infl.target.protein
  }
  def positiveInfluenceOnActiveState(infl: PositiveInfluenceOnState): PositiveInfluenceOnKinaseActivity = PositiveInfluenceOnActiveState(infl)


  trait Search[M[_]] { self: PositiveInfluenceOnState.Search[M] =>

    def Nuggets: proteinrefinery.lib.Nuggets[M]

    def positiveInfluenceOnKinaseActivityC(agent: Protein, kinase: Protein)(implicit M: Monad[M]): ContU[M, Ref] = for {
      ppref <- Nuggets.kinaseActivityC(kinase)
      res <- Antichain.map(Antichain.mapC(ppref)(pp => positiveInfluenceOnStateC(agent, pp)))(positiveInfluenceOnActiveState(_))
    } yield res

  }

}