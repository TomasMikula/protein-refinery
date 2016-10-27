package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Antichain, IncSet, Propagation}
import nutcracker.IncSet.IncSetRef
import nutcracker.util.ContU
import proteinrefinery.util.Tracking

import scalaz.{Monad, Show}

sealed trait NegativeInfluenceOnAssociation

object NegativeInfluenceOnAssociation {

  type Ref = Antichain.Ref[NegativeInfluenceOnAssociation]

  // Constructors

  case class ByCompetitiveBinding(value: CompetitiveBinding) extends NegativeInfluenceOnAssociation
  def        byCompetitiveBinding(value: CompetitiveBinding):        NegativeInfluenceOnAssociation = ByCompetitiveBinding(value)


  trait Search[M[_]] {

    implicit def Propagation: Propagation[M]
    implicit def Tracking: Tracking[M]
    def Nuggets: proteinrefinery.lib.Nuggets[M]

    // TODO: should return DSet
    def negativeInfluenceOnAssociation(p: Protein, a: Assoc)(implicit M: Monad[M]): M[IncSetRef[Ref]] =
      IncSet.collectAll(a.bindings.map(b => searchCompetitiveBinding(p, b)))

    def negativeInfluenceOnAssociationC(p: Protein, a: Assoc)(implicit M: Monad[M]): ContU[M, Ref] =
      ContU.sequence(a.bindings.map(b => searchCompetitiveBinding(p, b)))

    private def searchCompetitiveBinding(competitor: Protein, bnd: Binding)(implicit M: Monad[M]): ContU[M, Ref] = {
      val l = Antichain.map(competitiveBindings(competitor, bnd.leftPattern ))(cb => byCompetitiveBinding(CompetitiveBinding(bnd.flip, cb)))
      val r = Antichain.map(competitiveBindings(competitor, bnd.rightPattern))(cb => byCompetitiveBinding(CompetitiveBinding(bnd,      cb)))
      ContU.sequence(l, r)
    }

    // TODO: return Prg[DSet[Binding]] instead
    private def competitiveBindings(competitor: Protein, bp: BindingPartnerPattern)(implicit M: Monad[M]): ContU[M, Binding.Ref] =
      ContU.filterMap(Nuggets.bindingsOfC(competitor).flatMap(ref => ref.asCont[M].map((ref, _)))) {
        case (ref, bnd) => if(bnd.rightPattern overlaps bp) Option(ref) else None
      }

  }


  // Typeclass instances

  implicit def showInstance: Show[NegativeInfluenceOnAssociation] = new Show[NegativeInfluenceOnAssociation] {
    override def shows(x: NegativeInfluenceOnAssociation): String = x match {
      case ByCompetitiveBinding(cb) => Show[CompetitiveBinding].shows(cb)
    }
  }
}