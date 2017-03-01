package proteinrefinery

import nutcracker.{Defer, Propagation}
import nutcracker.util.{HEqualK, ShowK}
import proteinrefinery.util.Tracking
import scala.language.higherKinds
import scalaz.Id.Id
import scalaz.{Monad, StateT}

trait Refinery {
  type Prg[_]
  type Ref[_]

  implicit val prgMonad: Monad[Prg]
  implicit val refEquality: HEqualK[Ref]
  implicit val refShow: ShowK[Ref]
  implicit val propagationApi: Propagation[Prg, Ref]
  implicit val deferApi: Defer[Prg, Cost]
  implicit val trackingApi: Tracking[Prg, Ref]

  def fetch[A](ref: Ref[A])(s: State[Prg]): A
  def interpret[A](prg: Prg[A], s: State[Prg]): (State[Prg], A)

  def interpret[A](prg: Prg[A]): StateT[Id, State[Prg], A] = scalaz.State(s => interpret(prg, s))
  def interpret0[A](prg: Prg[A]): (State[Prg], A) = interpret(prg, emptyState[Prg])
  def interpretFetch0[A](prg: Prg[Ref[A]]): A = {
    val (s, ref) = interpret0(prg)
    fetch(ref)(s)
  }

  val lib: Lib[Prg, Ref]
}

object Refinery {
  type Aux[Prg0[_], Ref0[_]] = Refinery { type Prg[A] = Prg0[A]; type Ref[A] = Ref0[A] }
}
