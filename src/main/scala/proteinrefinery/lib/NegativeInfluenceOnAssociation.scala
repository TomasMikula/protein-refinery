package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Discrete, IncSet, Propagation}
import nutcracker.ops._
import nutcracker.util.{ContU, DeepEqualK, EqualK, IsEqual}
import proteinrefinery.util.Tracking

import scalaz.{Monad, Show}

sealed trait NegativeInfluenceOnAssociation[Ref[_]]

object NegativeInfluenceOnAssociation {

  type Ref[Var[_]] = Var[Discrete[NegativeInfluenceOnAssociation[Var]]]

  // Constructors

  case class ByCompetitiveBinding[Var[_]](value: CompetitiveBinding[Var]) extends NegativeInfluenceOnAssociation[Var]
  def        byCompetitiveBinding[Var[_]](value: CompetitiveBinding[Var]):        NegativeInfluenceOnAssociation[Var] = ByCompetitiveBinding(value)


  trait Search[M[_], Var[_], Val[_]] {

    protected implicit def Propagation: Propagation[M, Var, Val]
    implicit def Tracking: Tracking[M, Var, Val]
    def Nuggets: proteinrefinery.lib.Nuggets[M, Var, Val]
    def IncSets: nutcracker.IncSets[M, Var, Val]

    // TODO: should return DSet
    def negativeInfluenceOnAssociation(p: Protein, a: Assoc[Var])(implicit M: Monad[M], E: EqualK[Var]): M[Var[IncSet[NegativeInfluenceOnAssociation[Var]]]] =
      IncSets.collectAll(a.bindings.map(b => searchCompetitiveBinding(p, b)))

    def negativeInfluenceOnAssociationC(p: Protein, aRef: Assoc.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, NegativeInfluenceOnAssociation[Var]] =
      aRef.asCont_ flatMap (a => negativeInfluenceOnAssociationC(p, a))

    def negativeInfluenceOnAssociationC(p: Protein, a: Assoc[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, NegativeInfluenceOnAssociation[Var]] =
      ContU.sequence(a.bindings.map(b => searchCompetitiveBinding(p, b)))

    private def searchCompetitiveBinding(competitor: Protein, bnd: Binding[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, NegativeInfluenceOnAssociation[Var]] =
    bnd.witness.asCont_ flatMap { rule => {
      val l = competitiveBindings(competitor, bnd.getLeftPattern(rule) ).map(cb => byCompetitiveBinding(CompetitiveBinding(bnd.flip, cb)))
      val r = competitiveBindings(competitor, bnd.getRightPattern(rule)).map(cb => byCompetitiveBinding(CompetitiveBinding(bnd,      cb)))
      ContU.sequence(l, r)
    }}

    // TODO: return Prg[CellSet[Binding]] instead
    private def competitiveBindings(competitor: Protein, bp: BindingPartnerPattern[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Binding[Var]] =
      ContU.filterMap(Nuggets.bindingsOfC(competitor).flatMap(bnd => bnd.witness.asCont_.map((bnd, _)))) {
        case (bnd, rule) => if(bnd.getRightPattern(rule) overlaps bp) Option(bnd) else None
      }

  }


  // Typeclass instances

  implicit val deepEqualKInstance: DeepEqualK[NegativeInfluenceOnAssociation, NegativeInfluenceOnAssociation] =
    new DeepEqualK[NegativeInfluenceOnAssociation, NegativeInfluenceOnAssociation] {
      def equal[Ptr1[_], Ptr2[_]](a1: NegativeInfluenceOnAssociation[Ptr1], a2: NegativeInfluenceOnAssociation[Ptr2]): IsEqual[Ptr1, Ptr2] = {
        val (ByCompetitiveBinding(cb1), ByCompetitiveBinding(cb2)) = (a1, a2)
        IsEqual(cb1, cb2)
      }
    }

  implicit def showInstance[Var[_]]: Show[NegativeInfluenceOnAssociation[Var]] = new Show[NegativeInfluenceOnAssociation[Var]] {
    override def shows(x: NegativeInfluenceOnAssociation[Var]): String = x match {
      case ByCompetitiveBinding(cb) => Show[CompetitiveBinding[Var]].shows(cb)
    }
  }
}