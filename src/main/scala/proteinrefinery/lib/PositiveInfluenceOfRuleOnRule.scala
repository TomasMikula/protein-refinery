package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Discrete, Propagation}
import nutcracker.ops._
import nutcracker.util.{ContU, DeepShowK, EqualK, MonadObjectOutput}
import nutcracker.util.EqualK._
import nutcracker.util.ops.tell._

import scalaz.Monad
import scalaz.syntax.equal._
import scalaz.syntax.monad._

sealed trait PositiveInfluenceOfRuleOnRule[Ref[_]] {
  def target: Rule.Ref[Ref]
}

object PositiveInfluenceOfRuleOnRule {
  type Ref[Var[_]] = Var[Discrete[PositiveInfluenceOfRuleOnRule[Var]]]

  case class Enables[Var[_]](enabler: Rule.Ref[Var], enablee: Enablee[Var]) extends PositiveInfluenceOfRuleOnRule[Var] {
    def target = enablee.target
  }

  case class InAssoc[Var[_]](rule: Rule.Ref[Var], enablee: Enablee[Var]) extends PositiveInfluenceOfRuleOnRule[Var] {
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
        case PositiveInfluenceOfRuleOnRule.InAssoc(r, _) => r
      }
      def target = influence.target
      def containsEdge(enabler: Rule.Ref[Var], enablee: Rule.Ref[Var])(implicit E: EqualK[Var]): Boolean = influence match {
        case Enables(r, t) =>
          (enabler === r) && (t.link === enablee) || t.containsEdge(enabler, enablee)
        case PositiveInfluenceOfRuleOnRule.InAssoc(r, t) =>
          (enabler === r) && (t.link === enablee) || t.containsEdge(enabler, enablee)
      }
    }
    case class InAssoc[Var[_]](rule: Rule.Ref[Var], assoc: Assoc.Ref[Var], enablee: Enablee[Var]) extends Enablee[Var] {
      def link = rule

      def target = enablee.target

      def containsEdge(enabler: Rule.Ref[Var], enablee: Rule.Ref[Var])(implicit E: EqualK[Var]): Boolean =
        (enabler === this.rule) && (enablee === this.enablee.link) || this.enablee.containsEdge(enabler, enablee)
    }

    implicit val deepShowK: DeepShowK[Enablee] = new DeepShowK[Enablee] {
      def show[Ptr[_], M[_]](a: Enablee[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] = a match {
        case Singleton(target) => M.writeObject(target)
        case Indirect(infl) => M(infl)
        case InAssoc(rule, assoc, enablee) => tell"${M.writeObject(rule)} facilitates association in ${M.nest(M(enablee))}"
      }
    }
  }

  implicit val deepShowK: DeepShowK[PositiveInfluenceOfRuleOnRule] = new DeepShowK[PositiveInfluenceOfRuleOnRule] {
    def show[Ptr[_], M[_]](a: PositiveInfluenceOfRuleOnRule[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      a match {
        case Enables(enabler, enablee) => tell"${M.writeObject(enabler)} enables ${M.nest(M(enablee))}"
        case InAssoc(rule, enablee) => tell"${M.writeObject(rule)} facilitates association in ${M.nest(M(enablee))}"
      }
  }

  trait Search[M[_], Var[_], Val[_]] {
    protected implicit def Propagation: Propagation[M, Var, Val]
    def RuleOps: Rule.Ops[M, Var, Val]

    def positiveInfluenceOfRuleOnRule(r: Rule.Ref[Var], t: Rule.Ref[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, PositiveInfluenceOfRuleOnRule[Var]] =
      positiveInfluenceOfRuleOnRule(r, Enablee.Singleton(t))

    private def positiveInfluenceOfRuleOnRule(r: Rule.Ref[Var], t: Enablee[Var])(implicit M: Monad[M], E: EqualK[Var]): ContU[M, PositiveInfluenceOfRuleOnRule[Var]] = {
      ContU.sequence(
        RuleOps.enablersOfC(t.link) >>= { s =>
          if(s === r) ContU.point(Enables(r, t))
          else if(t.containsEdge(s, t.link)) ContU.noop
          else positiveInfluenceOfRuleOnRule(r, Enablee.Indirect(Enables(s, t)))
        },
        for {
          aRef <- RuleOps.associationsOfC(t.link)
          a <- aRef.asCont_
          infl <- ContU.sequence(a.bindings.map(b => {
            val w = b.witness
            if(w === r) ContU.point[M, PositiveInfluenceOfRuleOnRule[Var]](InAssoc(r, t))
            else positiveInfluenceOfRuleOnRule(r, Enablee.InAssoc(w, aRef, t))
          }))
        } yield infl
      )
    }

  }

}