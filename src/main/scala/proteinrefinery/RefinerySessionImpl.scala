package proteinrefinery

import nutcracker.util.CoproductK.:++:
import nutcracker.util.KPair._
import nutcracker.util.{FreeKT, HEqualK, ShowK, StateInterpreter}
import scalaz.Monad

class RefinerySessionImpl[State1[_[_]], State2[_[_]], Ref0[_]](
  val refinery: Refinery { type Ref[A] = Ref0[A]; type State[K[_]] = State1[K] },
  val goalModule: GoalKeepingModule[Ref0] { type State[K[_]] = State2[K] }
) extends RefinerySession {

  type Ref[A] = Ref0[A]
  type Lang[K[_], A] = (refinery.Lang :++: goalModule.Lang)#Out[K, A]
  type State[K[_]]   = (State1        :**:          State2)#Out[K]

  implicit val prgMonad: Monad[Prg] = FreeKT.freeKTMonad
  implicit val refEquality: HEqualK[Ref] = refinery.refEquality
  implicit val refShow: ShowK[Ref] = refinery.refShow

  protected implicit val goalKeepingApi: GoalKeeping[Prg, Ref] = goalModule.freeGoalKeeping

  val interpreter: StateInterpreter[Lang, State] = refinery.interpreter :&&: goalModule.interpreter
  private val prgInterpreter = interpreter.freeInstance

  protected var state: State[Prg] = empty[Prg]

  def empty[K[_]]: State[K] = refinery.empty[K] :*: goalModule.empty[K]

  def fetch[A](ref: Ref[A]): A =
    refinery.fetch(ref, state._1)

  def interpret[A](prg: Prg[A]): A = {
    val (s, a) = prgInterpreter(prg).run(state)
    state = s
    a
  }

  val lib: Lib[Prg, Ref] = refinery.freeLib[Lang]
}
