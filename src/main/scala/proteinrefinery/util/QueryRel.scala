package proteinrefinery.util

import nutcracker.Dom
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

trait QueryRel[Q, A] {
  import QueryRel._

  def apply(q: Q, a: A): MatchResult
}

object QueryRel {
  sealed trait MatchResult {
    def isMatch: Boolean = this == Match
  }
  case object Match extends MatchResult
  case object NoMatch extends MatchResult
  case object Irreconcilable extends MatchResult // when the element cannot be refined to match the query

  trait Laws[Q, A] {
    type UpdateQ
    type UpdateA

    def queryRel: QueryRel[Q, A]
    def domQ: Dom.Aux[Q, UpdateQ, _]
    def domA: Dom.Aux[A, UpdateA, _]

    /** Elements selected by a query form an upward closed set. */
    def queryUpwardClosedness(q: Q, a: A, u: UpdateA): Boolean =
      !queryRel(q, a).isMatch || queryRel(q, domA.update_(a, u)).isMatch

    /** Queries that match a given element form a downward closed set. */
    def elemDownwardClosedness(a: A, q: Q, u: UpdateQ): Boolean =
      !queryRel(domQ.update_(q, u), a).isMatch || queryRel(q, a).isMatch
  }

  object Laws {
    type Aux[Q, A, UQ, UA] = Laws[Q, A] { type UpdateQ = UQ; type UpdateA = UA }
  }

  def laws[Q, A](rel: QueryRel[Q, A])(implicit dq: Dom[Q], da: Dom[A]): Laws.Aux[Q, A, dq.Update, da.Update] =
    new Laws[Q, A] {
      type UpdateQ = dq.Update
      type UpdateA = da.Update

      val queryRel = rel
      def domQ = dq
      def domA = da
    }

  def properties[Q, A](rel: QueryRel[Q, A])(implicit dq: Dom[Q], da: Dom[A]): PropBuilder[Q, A, dq.Update, da.Update] =
    PropBuilder(rel)(dq, da)

  final case class PropBuilder[Q, A, UQ, UA](rel: QueryRel[Q, A])(implicit dq: Dom.Aux[Q, UQ, _], da: Dom.Aux[A, UA, _]) {
    def apply(name: String)(implicit arbQ: Arbitrary[Q], arbA: Arbitrary[A], arbUQ: Arbitrary[UQ], arbUA: Arbitrary[UA]): Properties =
      new Properties(name) {
        val laws = QueryRel.laws(rel)

        property("queryUpwardClosedness")  = forAll(laws.queryUpwardClosedness _)
        property("elemDownwardClosedness") = forAll(laws.elemDownwardClosedness _)
      }
  }
}
