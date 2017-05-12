package proteinrefinery

import nutcracker.util.CoproductK.:++:
import nutcracker.util.KPair._
import nutcracker.util.{HOrderK, ShowK, StateInterpreter}
import scalaz.Monad

class RefinerySessionImpl[State1[_[_]], State2[_[_]], Ref0[_[_], _], Val0[_[_], _]](
  val refinery: Refinery { type VarK[K[_], A] = Ref0[K, A]; type ValK[K[_], A] = Val0[K, A]; type StateK[K[_]] = State1[K] },
  val goalModule: GoalKeepingModule[Ref0] { type StateK[K[_]] = State2[K] }
) extends RefinerySession {

  type VarK[K[_], A] = Ref0[K, A]
  type ValK[K[_], A] = Val0[K, A]
  type Lang[K[_], A] = (refinery.Lang :++: goalModule.Lang)#Out[K, A]
  type StateK[K[_]]  = (State1        :**:          State2)#Out[K]

  override def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A] = refinery.readOnlyK(ref)
  override val prgMonad: Monad[Prg] = implicitly
  implicit def varOrderK[K[_]]: HOrderK[VarK[K, ?]] = refinery.varOrderK
  implicit def varShowK[K[_]]: ShowK[VarK[K, ?]] = refinery.varShowK
  implicit def valOrderK[K[_]]: HOrderK[ValK[K, ?]] = refinery.valOrderK
  implicit def valShowK[K[_]]: ShowK[ValK[K, ?]] = refinery.valShowK

  protected implicit val goalKeepingApi: GoalKeeping[Prg, Var] = goalModule.freeGoalKeeping

  override val stepInterpreter: StateInterpreter[Prg, Lang[Prg, ?], State] = refinery.interpreter[Prg, State] :+: goalModule.interpreter[Prg, State]

  protected var state: StateK[Prg] = emptyK[Prg]

  def emptyK[K[_]]: StateK[K] = refinery.emptyK[K] :*: goalModule.emptyK[K]
  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] = refinery.fetchK(ref, s._1)
  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A = refinery.fetchK(ref, s._1)

  def fetch[A](ref: Var[A]): A =
    refinery.fetchK(ref, state._1)

  def interpret[A](prg: Prg[A]): A = {
    val (s, a) = interpret(prg, state)
    state = s
    a
  }

  val lib: Lib[Prg, Var, Val] = refinery.freeLib[Lang]
}
