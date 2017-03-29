package proteinrefinery

import nutcracker.rel.{RelModule, Relations}
import nutcracker.util.CoproductK.{:++:, :+:}
import nutcracker.util.KPair.{:**:, :*:, _}
import nutcracker.{Defer, DeferModule, Propagation, PropagationModule, RefBundle}
import nutcracker.util.{FreeK, FreeKT, HEqualK, HOrderK, InjectK, ShowK, StateInterpreter}
import proteinrefinery.util.{Tracking, TrackingModule}
import scala.language.higherKinds
import scalaz.Id.Id
import scalaz.{Monad, StateT, ~>}

trait Refinery extends RefBundle {

  implicit val prgMonad: Monad[Prg]
  implicit val refEquality: HEqualK[Ref]
  implicit val refOrder: HOrderK[Ref]
  implicit val refShow: ShowK[Ref]

  def freePropagation[F[_[_], _]](implicit i: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref]
  def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], Cost]
  def freeTrackingApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], Ref]
  def freeRelationsApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Relations[FreeK[F, ?]]

  implicit val propagationApi: Propagation[Prg, Ref] = freePropagation[Lang]
  implicit val deferApi: Defer[Prg, Cost] = freeDeferApi[Lang]
  implicit val trackingApi: Tracking[Prg, Ref] = freeTrackingApi[Lang]
  implicit val relationsApi: Relations[Prg] = freeRelationsApi[Lang]

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

private[proteinrefinery] class RefineryImpl[Ref0[_], PropState[_[_]], RelState[_[_]], TrackState[_[_]], DeferState[_[_]]](
  val propMod: PropagationModule { type Ref[A] = Ref0[A]; type State[K[_]] = PropState[K] },
  val relMod: RelModule { type State[K[_]]  = RelState[K] },
  val trckMod: TrackingModule[Ref0] { type State[K[_]] = TrackState[K] },
  val defMod: DeferModule[Cost] { type State[K[_]] = DeferState[K] }
) extends Refinery {
  type Ref[A] = Ref0[A]
  type Lang[K[_], A] = (propMod.Lang :+: relMod.Lang :+: trckMod.Lang :++: defMod.Lang)#Out[K, A]
  type State[K[_]]   = (PropState    :*: RelState    :*: TrackState   :**: DeferState )#Out[K]

  private implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] = FreeKT.freeKTMonad[F, Id] // https://issues.scala-lang.org/browse/SI-10238

  val prgMonad: Monad[Prg] = freeKMonad[Lang]
  implicit val refEquality: HEqualK[Ref] = propMod.refEquality
  implicit val refOrder: HOrderK[Ref] = propMod.refOrder
  implicit val refShow: ShowK[Ref] = propMod.refShow

  implicit def freePropagation[F[_[_], _]](implicit i: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref] = propMod.freePropagation[F](i.compose[propMod.Lang])
  implicit def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], Cost] = defMod.freeDeferApi[F](i.compose[defMod.Lang](InjectK.injectRight(InjectK.injectRight(InjectK.injectRight))))
  implicit def freeTrackingApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], Ref] = trckMod.freeTracking[F](i.compose[trckMod.Lang])
  implicit def freeRelationsApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Relations[FreeK[F, ?]] = relMod.freeRelations[F](i.compose[relMod.Lang])

  def freeLib[F[_[_], _]](implicit i: InjectK[Lang, F]): Lib[FreeK[F, ?], Ref] = new Lib[FreeK[F, ?], Ref]

  val interpreter: StateInterpreter[Lang, State] = propMod.interpreter :&: relMod.interpreter :&: trckMod.interpreter :&&: defMod.interpreter
  private val interpreterF = interpreter.freeInstance

  def empty[K[_]]: State[K] = propMod.empty[K] :*: relMod.empty[K] :*: trckMod.empty[K] :*: defMod.empty[K]
  def fetch[K[_], D](ref: Ref[D], s: State[K]): D = propMod.fetch(ref, s._1)
  def interpret[A](prg: Prg[A], s: State[Prg]): (State[Prg], A) = interpreterF(prg)(s)

  val lib: Lib[Prg, Ref] =
  // all of the arguments are implicit, but scalac...
  new Lib[Prg, Ref]()(deferApi, propagationApi, trackingApi, prgMonad, refEquality.homogenize)
}

object RefineryImpl {
  def apply(propMod: PropagationModule, relMod: RelModule)(trckMod: TrackingModule[propMod.Ref], defMod: DeferModule[Cost]): RefineryImpl[propMod.Ref, propMod.State, relMod.State, trckMod.State, defMod.State] =
    new RefineryImpl[propMod.Ref, propMod.State, relMod.State, trckMod.State, defMod.State](propMod, relMod, trckMod, defMod)
}