package proteinrefinery

import nutcracker.rel.{RelModule, Relations}
import nutcracker.util.CoproductK.{:++:, :+:}
import nutcracker.util.KPair.{:**:, :*:, _}
import nutcracker.{Defer, DeferModule, Propagation, PropagationModule, RefBundle}
import nutcracker.util.{FreeK, FreeKT, HOrderK, InjectK, ShowK, StateInterpreter}
import proteinrefinery.util.{Tracking, TrackingModule}
import scala.language.higherKinds
import scalaz.Id.Id
import scalaz.{Monad, StateT, ~>}

trait Refinery extends RefBundle {

  implicit val prgMonad: Monad[Prg]

  def freePropagation[F[_[_], _]](implicit i: InjectK[Lang, F]): Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
  def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], Cost]
  def freeTrackingApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
  def freeRelationsApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Relations[FreeK[F, ?]]

  implicit val propagationApi: Propagation[Prg, Var, Val] = freePropagation[Lang]
  implicit val deferApi: Defer[Prg, Cost] = freeDeferApi[Lang]
  implicit val trackingApi: Tracking[Prg, Var, Val] = freeTrackingApi[Lang]
  implicit val relationsApi: Relations[Prg] = freeRelationsApi[Lang]

  val interpreter: StateInterpreter[Lang, StateK]

  def interpret[A](prg: Prg[A], s: State): (State, A)

  def interpretFetch[A](prg: Prg[Var[A]], s: State): (State, A) = {
    val (s1, ref) = interpret(prg, s)
    (s1, fetch(ref, s1))
  }

  def fetch[A](ref: Var[A]): StateT[Id, State, A] = scalaz.State(s => (s, fetch(ref, s)))
  def interpret[A](prg: Prg[A]): StateT[Id, State, A] = scalaz.State(s => interpret(prg, s))
  def interpretFetch[A](prg: Prg[Var[A]]): StateT[Id, State, A] = scalaz.State(s => interpretFetch(prg, s))
  def interpretFetch0[A](prg: Prg[Var[A]]): A = {
    val (s, ref) = interpret0(prg)
    fetch(ref, s)
  }
  def run[A](f: StateT[Id, State, A]): (State, A) = f.run(empty)
  def eval[A](f: StateT[Id, State, A]): A = f.eval(empty)

  def fetcher(s: State): Var ~> Id = λ[Var ~> Id](fetch(_, s))

  val lib: Lib[Prg, Var, Val]
  def freeLib[F[_[_], _]](implicit i: InjectK[Lang, F]): Lib[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
}

object Refinery {
  type Aux[Lang0[_[_], _], State0[K[_]], Var0[_]] = Refinery {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
    type Var[A] = Var0[A]
  }
}

private[proteinrefinery] class RefineryImpl[Var0[_[_], _], Val0[_[_], _], PropState[_[_]], RelState[_[_]], TrackState[_[_]], DeferState[_[_]]](
  val propMod: PropagationModule { type VarK[K[_], A] = Var0[K, A]; type ValK[K[_], A] = Val0[K, A]; type StateK[K[_]] = PropState[K] },
  val relMod: RelModule { type StateK[K[_]]  = RelState[K] },
  val trckMod: TrackingModule[Var0, Val0] { type StateK[K[_]] = TrackState[K] },
  val defMod: DeferModule[Cost] { type StateK[K[_]] = DeferState[K] }
) extends Refinery {
  type VarK[K[_], A] = Var0[K, A]
  type ValK[K[_], A] = Val0[K, A]
  type Lang[K[_], A] = (propMod.Lang :+: relMod.Lang :+: trckMod.Lang :++: defMod.Lang)#Out[K, A]
  type StateK[K[_]]  = (PropState    :*: RelState    :*: TrackState   :**: DeferState )#Out[K]

  private implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] = FreeKT.freeKTMonad[F, Id] // https://issues.scala-lang.org/browse/SI-10238

  val prgMonad: Monad[Prg] = freeKMonad[Lang]
  implicit def varOrderK[K[_]]: HOrderK[VarK[K, ?]] = propMod.varOrderK
  implicit def varShowK[K[_]]: ShowK[VarK[K, ?]] = propMod.varShowK
  implicit def valOrderK[K[_]]: HOrderK[ValK[K, ?]] = propMod.valOrderK
  implicit def valShowK[K[_]]: ShowK[ValK[K, ?]] = propMod.valShowK

  override def readOnly[A](ref: Var[A]): Val[A] = propagationApi.readOnly(ref)

  implicit def freePropagation[F[_[_], _]](implicit i: InjectK[Lang, F]): Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] = propMod.freePropagation[F](i.compose[propMod.Lang])
  implicit def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], Cost] = defMod.freeDeferApi[F](i.compose[defMod.Lang](InjectK.injectRight(InjectK.injectRight(InjectK.injectRight))))
  implicit def freeTrackingApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] = trckMod.freeTracking[F](i.compose[trckMod.Lang])
  implicit def freeRelationsApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Relations[FreeK[F, ?]] = relMod.freeRelations[F](i.compose[relMod.Lang])

  def freeLib[F[_[_], _]](implicit i: InjectK[Lang, F]): Lib[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] = new Lib[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]

  val interpreter: StateInterpreter[Lang, StateK] = propMod.interpreter :&: relMod.interpreter :&: trckMod.interpreter :&&: defMod.interpreter
  private val interpreterF = interpreter.freeInstance

  def emptyK[K[_]]: StateK[K] = propMod.emptyK[K] :*: relMod.emptyK[K] :*: trckMod.emptyK[K] :*: defMod.emptyK[K]
  def fetchK[K[_], D](ref: ValK[K, D], s: StateK[K]): Option[D] = propMod.fetchK(ref, s._1)
  def fetchK[K[_], D](ref: VarK[K, D], s: StateK[K]):        D  = propMod.fetchK(ref, s._1)
  def interpret[A](prg: Prg[A], s: State): (State, A) = interpreterF(prg)(s)

  val lib: Lib[Prg, Var, Val] =
  // all of the arguments are implicit, but scalac...
  new Lib[Prg, Var, Val]()(deferApi, propagationApi, trackingApi, prgMonad, varOrder.homogenize)
}

object RefineryImpl {
  def apply(propMod: PropagationModule, relMod: RelModule)(trckMod: TrackingModule[propMod.VarK, propMod.ValK], defMod: DeferModule[Cost]): RefineryImpl[propMod.VarK, propMod.ValK, propMod.StateK, relMod.StateK, trckMod.StateK, defMod.StateK] =
    new RefineryImpl[propMod.VarK, propMod.ValK, propMod.StateK, relMod.StateK, trckMod.StateK, defMod.StateK](propMod, relMod, trckMod, defMod)
}