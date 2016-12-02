package proteinrefinery.lib

import scala.language.higherKinds

import nutcracker.{Antichain, Propagation}
import nutcracker.util.{ContU, EqualK}
import nutcracker.util.EqualK._

import scalaz.Monad
import scalaz.syntax.equal._
import scalaz.syntax.monad._

sealed trait PositiveInfluenceOfRuleOnRule[Ref[_]] {
  def target: Rule.Ref[Ref]
}

object PositiveInfluenceOfRuleOnRule {
  type Ref[Var[_]] = Var[Antichain[PositiveInfluenceOfRuleOnRule[Var]]]

  case class Enables[Var[_]](enabler: Rule.Ref[Var], enablee: Enablee[Var]) extends PositiveInfluenceOfRuleOnRule[Var] {
    def target = enablee.target
  }

  sealed trait Enablee[Var[_]] {
    def link: Rule.Ref[Var]
    def target: Rule.Ref[Var]

    def containsEdge(enabler: Rule.Ref[Var], enablee: Rule.Ref[Var])(implicit E: EqualK[Var]): Boolean
  }
  object Enablee {
    case class Singleton[Var[_]](target: Rule.Ref[Var]) extends Enablee[Var] {
      def link = target
      def containsEdge(enabler: Rule.Ref[Var], enablee: Rule.Ref[Var])(implicit E: EqualK[Var]): Boolean = false
    }
    case class Indirect[Var[_]](influence: PositiveInfluenceOfRuleOnRule[Var]) extends Enablee[Var] {
      def link = influence match {
        case Enables(r, _) => r
      }
      def target = influence.target
      def containsEdge(enabler: Rule.Ref[Var], enablee: Rule.Ref[Var])(implicit E: EqualK[Var]): Boolean = influence match {
        case Enables(r, t) =>
          (enabler === r) && (t.link === enablee) || t.containsEdge(enabler, enablee)
      }
    }
  }

  trait Search[M[_], Var[_]] {
    implicit def Propagation: Propagation[M, Var]
    def RuleOps: Rule.Ops[M, Var]

    private def positiveInfluenceOfRuleOnRule(r: Rule.Ref[Var], t: Enablee[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, Ref[Var]] = {
      RuleOps.enablersOfC(t.link) >>= { s =>
        if(s === r) Antichain.cellC[M, Var, PositiveInfluenceOfRuleOnRule[Var]](Enables(r, t))
        else if(t.containsEdge(s, t.link)) ContU.noop
        else positiveInfluenceOfRuleOnRule(r, Enablee.Indirect(Enables(s, t)))
      }
    }

  }

}