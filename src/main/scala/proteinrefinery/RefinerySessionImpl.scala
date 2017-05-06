package proteinrefinery

import nutcracker.util.CoproductK.:++:
import nutcracker.util.KPair._
import nutcracker.util.{FreeKT, HOrderK, ShowK, StateInterpreter}
import scalaz.Monad

class RefinerySessionImpl[State1[_[_]], State2[_[_]], Ref0[_[_], _], Val0[_[_], _]](
  val refinery: Refinery { type VarK[K[_], A] = Ref0[K, A]; type ValK[K[_], A] = Val0[K, A]; type StateK[K[_]] = State1[K] },
  val goalModule: GoalKeepingModule[Ref0] { type StateK[K[_]] = State2[K] }
) extends RefinerySession {

  type VarK[K[_], A] = Ref0[K, A]
  type ValK[K[_], A] = Val0[K, A]
  type Lang[K[_], A] = (refinery.Lang :++: goalModule.Lang)#Out1[K, A]
  type StateK[K[_]]  = (State1        :**:          State2)#Out[K]

  implicit val prgMonad: Monad[Prg] = FreeKT.freeKTMonad
  implicit def varOrderK[K[_]]: HOrderK[VarK[K, ?]] = refinery.varOrderK
  implicit def varShowK[K[_]]: ShowK[VarK[K, ?]] = refinery.varShowK

  protected implicit val goalKeepingApi: GoalKeeping[Prg, Var] = goalModule.freeGoalKeeping

  val interpreter: StateInterpreter[Prg, Lang[Prg, ?], State] = refinery.interpreter[Prg, State] :+: goalModule.interpreter[Prg, State]
  private val prgInterpreter = interpreter.freeInstance(_.run.toFree)

  protected var state: StateK[Prg] = empty[Prg]

  def empty[K[_]]: StateK[K] = refinery.emptyK[K] :*: goalModule.emptyK[K]

  def fetch[A](ref: Var[A]): A =
    refinery.fetchK(ref, state._1)

  def interpret[A](prg: Prg[A]): A = {
    val (s, a) = prgInterpreter(prg.run.toFree).run(state)
    state = s
    a
  }

  val lib: Lib[Prg, Var, Val] = refinery.freeLib[Lang]
}
