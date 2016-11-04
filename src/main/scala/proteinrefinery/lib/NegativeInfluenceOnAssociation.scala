package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker._
import nutcracker.util.{ContU, EqualK}
import proteinrefinery.util.Tracking

import scalaz.{Monad, Show}

sealed trait NegativeInfluenceOnAssociation[Ref[_]]

object NegativeInfluenceOnAssociation {

  type Ref[Var[_]] = Var[Antichain[NegativeInfluenceOnAssociation[Var]]]

  // Constructors

  case class ByCompetitiveBinding[Var[_]](value: CompetitiveBinding[Var]) extends NegativeInfluenceOnAssociation[Var]
  def        byCompetitiveBinding[Var[_]](value: CompetitiveBinding[Var]):        NegativeInfluenceOnAssociation[Var] = ByCompetitiveBinding(value)


  trait Search[M[_], Var[_]] {

    implicit def Propagation: Propagation[M, Var]
    implicit def Tracking: Tracking[M, Var]
    def Nuggets: proteinrefinery.lib.Nuggets[M, Var]
    def IncSets: nutcracker.IncSets[M, Var]

    // TODO: should return DSet
    def negativeInfluenceOnAssociation(p: Protein, a: Assoc[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[Ref[Var]]]] =
      IncSets.collectAll(a.bindings.map(b => searchCompetitiveBinding(p, b)))

    def negativeInfluenceOnAssociationC(p: Protein, a: Assoc[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] =
      ContU.sequence(a.bindings.map(b => searchCompetitiveBinding(p, b)))

    private def searchCompetitiveBinding(competitor: Protein, bnd: Binding[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
      val l = Antichain.map(competitiveBindings(competitor, bnd.leftPattern ))(cb => byCompetitiveBinding(CompetitiveBinding(bnd.flip, cb)))
      val r = Antichain.map(competitiveBindings(competitor, bnd.rightPattern))(cb => byCompetitiveBinding(CompetitiveBinding(bnd,      cb)))
      ContU.sequence(l, r)
    }

    // TODO: return Prg[DSet[Binding]] instead
    private def competitiveBindings(competitor: Protein, bp: BindingPartnerPattern[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Binding.Ref[Var]] =
      ContU.filterMap(Nuggets.bindingsOfC(competitor).flatMap(ref => ref.asCont[M].map((ref, _)))) {
        case (ref, bnd) => if(bnd.rightPattern overlaps bp) Option(ref) else None
      }

  }


  // Typeclass instances

  implicit def showInstance[Var[_]]: Show[NegativeInfluenceOnAssociation[Var]] = new Show[NegativeInfluenceOnAssociation[Var]] {
    override def shows(x: NegativeInfluenceOnAssociation[Var]): String = x match {
      case ByCompetitiveBinding(cb) => Show[CompetitiveBinding[Var]].shows(cb)
    }
  }
}