package proteinrefinery

import nutcracker.{Defer, Propagation, Relations}
import nutcracker.toolkit.{DeferModule, FreeRefToolkit, PropagationModule, RelModule}
import nutcracker.util.{FreeK, HOrderK, Inject, KPair, ShowK, StateInterpreter}
import nutcracker.util.CoproductK.{:++:, :+:}
import nutcracker.util.KPair._
import proteinrefinery.util.{Tracking, TrackingModule}
import scalaz.Id.Id
import scalaz.{Lens, Monad, StateT, Store, ~>}

trait Refinery extends FreeRefToolkit {

  implicit val prgMonad: Monad[Prg]

  type Inj[F[_[_], _]] = Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]

  def freePropagation[F[_[_], _]](implicit i: Inj[F]): Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
  def freeDeferApi[F[_[_], _]](implicit i: Inj[F]): Defer[FreeK[F, ?], Cost]
  def freeTrackingApi[F[_[_], _]](implicit i: Inj[F]): Tracking[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
  def freeRelationsApi[F[_[_], _]](implicit i: Inj[F]): Relations[FreeK[F, ?]]

  implicit val propagationApi: Propagation[Prg, Var, Val] = freePropagation[Lang]
  implicit val deferApi: Defer[Prg, Cost] = freeDeferApi[Lang]
  implicit val trackingApi: Tracking[Prg, Var, Val] = freeTrackingApi[Lang]
  implicit val relationsApi: Relations[Prg] = freeRelationsApi[Lang]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S]

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
  def freeLib[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Lib[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
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

  override val prgMonad: Monad[Prg] = implicitly
  implicit def varOrderK[K[_]]: HOrderK[VarK[K, ?]] = propMod.varOrderK
  implicit def varShowK[K[_]]: ShowK[VarK[K, ?]] = propMod.varShowK
  implicit def valOrderK[K[_]]: HOrderK[ValK[K, ?]] = propMod.valOrderK
  implicit def valShowK[K[_]]: ShowK[ValK[K, ?]] = propMod.valShowK

  override def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A] = propMod.readOnlyK(ref)

  implicit def freePropagation[F[_[_], _]](implicit i: Inj[F]): Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] =
    propMod.freePropagation[F](i.compose[propMod.Lang[FreeK[F, ?], ?]])

  implicit def freeDeferApi[F[_[_], _]](implicit i: Inj[F]): Defer[FreeK[F, ?], Cost] =
    defMod.freeDeferApi[F](i.compose[defMod.Lang[FreeK[F, ?], ?]])

  implicit def freeTrackingApi[F[_[_], _]](implicit i: Inj[F]): Tracking[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] =
    trckMod.freeTracking[F](i.compose[trckMod.Lang[FreeK[F, ?], ?]])

  implicit def freeRelationsApi[F[_[_], _]](implicit i: Inj[F]): Relations[FreeK[F, ?]] =
    relMod.freeRelations[F](i.compose[relMod.Lang[FreeK[F, ?], ?]])

  def freeLib[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Lib[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] =
    new Lib[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]

  // scalac could find all of these implicitly, but it doesn't ¯\_(ツ)_/¯
  private def propLens[K[_]]:  Lens[StateK[K], PropState[K]]  = KPair.fstLens
  private def relLens[K[_]]:   Lens[StateK[K], RelState[K]]   = implicitly[Lens[StateK[K], RelState[K]]]
  private def trackLens[K[_]]: Lens[StateK[K], TrackState[K]] = Lens(s => Store(t => s._1 :*: s._2._1 :*: t :*: s._2._2._2, s._2._2._1))
  private def deferLens[K[_]]: Lens[StateK[K], DeferState[K]] = Lens(s => Store(t => s._1 :*: s._2._1 :*: s._2._2._1 :*: t, s._2._2._2))

  override val stepInterpreter: StateInterpreter[Prg, Lang[Prg, ?], State] = interpreter[Prg, State](Lens.lensId[State])

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] = (
    propMod.stepInterpreterK[K, S](propLens[K].compose(lens)) :+:
      relMod.interpreter[K, S](relLens[K].compose(lens)) :+:
      trckMod.interpreter[K, S](trackLens[K].compose(lens)) :+:
      defMod.interpreter[K, S](deferLens[K].compose(lens))
  )

  def emptyK[K[_]]: StateK[K] = propMod.emptyK[K] :*: relMod.emptyK[K] :*: trckMod.emptyK[K] :*: defMod.emptyK[K]
  def fetchK[K[_], D](ref: ValK[K, D], s: StateK[K]): Option[D] = propMod.fetchK(ref, s._1)
  def fetchK[K[_], D](ref: VarK[K, D], s: StateK[K]):        D  = propMod.fetchK(ref, s._1)

  val lib: Lib[Prg, Var, Val] =
  // all of the arguments are implicit, but scalac...
  new Lib[Prg, Var, Val]()(deferApi, propagationApi, trackingApi, prgMonad, varOrder.homogenize)
}

object RefineryImpl {
  def apply(propMod: PropagationModule, relMod: RelModule)(trckMod: TrackingModule[propMod.VarK, propMod.ValK], defMod: DeferModule[Cost]): RefineryImpl[propMod.VarK, propMod.ValK, propMod.StateK, relMod.StateK, trckMod.StateK, defMod.StateK] =
    new RefineryImpl[propMod.VarK, propMod.ValK, propMod.StateK, relMod.StateK, trckMod.StateK, defMod.StateK](propMod, relMod, trckMod, defMod)
}