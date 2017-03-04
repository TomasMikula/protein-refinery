package proteinrefinery

import nutcracker.{Defer, Propagation, RefBundle, StashRestore}
import nutcracker.util.{FreeK, HEqualK, InjectK, ShowK, StateInterpreter}
import proteinrefinery.util.Tracking
import scala.language.higherKinds
import scalaz.Id.Id
import scalaz.{Monad, StateT, ~>}

trait Refinery extends RefBundle {

  implicit val prgMonad: Monad[Prg]
  implicit val refEquality: HEqualK[Ref]
  implicit val refShow: ShowK[Ref]

  def freePropagation[F[_[_], _]](implicit i: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref]
  def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], Cost]
  def freeTrackingApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], Ref]

  implicit val propagationApi: Propagation[Prg, Ref] = freePropagation[Lang]
  implicit val deferApi: Defer[Prg, Cost] = freeDeferApi[Lang]
  implicit val trackingApi: Tracking[Prg, Ref] = freeTrackingApi[Lang]

  val interpreter: StateInterpreter[Lang, State]

  def fetch[K[_], A](ref: Ref[A], s: State[K]): A
  def interpret[A](prg: Prg[A], s: State[Prg]): (State[Prg], A)

  def interpretFetch[A](prg: Prg[Ref[A]], s: State[Prg]): (State[Prg], A) = {
    val (s1, ref) = interpret(prg, s)
    (s1, fetch(ref, s1))
  }

  def fetch[A](ref: Ref[A]): StateT[Id, State[Prg], A] = scalaz.State(s => (s, fetch(ref, s)))
  def interpret[A](prg: Prg[A]): StateT[Id, State[Prg], A] = scalaz.State(s => interpret(prg, s))
  def interpretFetch[A](prg: Prg[Ref[A]]): StateT[Id, State[Prg], A] = scalaz.State(s => interpretFetch(prg, s))
  def interpret0[A](prg: Prg[A]): (State[Prg], A) = interpret(prg, empty[Prg])
  def interpretFetch0[A](prg: Prg[Ref[A]]): A = {
    val (s, ref) = interpret0(prg)
    fetch(ref, s)
  }
  def run[A](f: StateT[Id, State[Prg], A]): (State[Prg], A) = f.run(empty[Prg])
  def eval[A](f: StateT[Id, State[Prg], A]): A = f.eval(empty[Prg])

  def fetcher(s: State[Prg]): Ref ~> Id = λ[Ref ~> Id](fetch(_, s))

  val lib: Lib[Prg, Ref]
  def freeLib[F[_[_], _]](implicit i: InjectK[Lang, F]): Lib[FreeK[F, ?], Ref]
}

object Refinery {
  type Aux[Lang0[_[_], _], State0[K[_]], Ref0[_]] = Refinery {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
    type Ref[A] = Ref0[A]
  }
}

trait StashRefinery extends Refinery {
  implicit def stashRestore[K[_]]: StashRestore[State[K]]
}

object StashRefinery {
  type Aux[Lang0[_[_], _], State0[_[_]], Ref0[_]] = StashRefinery {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
    type Ref[A] = Ref0[A]
  }
}