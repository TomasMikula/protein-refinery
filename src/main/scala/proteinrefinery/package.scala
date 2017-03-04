import scala.language.higherKinds
import nutcracker._
import nutcracker.Propagation.{module => Prop}
import nutcracker.util.{FreeK, FreeKT, HEqualK, InjectK, ShowK, StateInterpreter}
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._
import proteinrefinery.util.{TrackLang, Tracker, Tracking}
import scalaz.Id._
import scalaz.Monad

package object proteinrefinery extends ImplicitConversions {

  private val Def = DeferModule.instance[Cost]

  def refinery(): Refinery = new Refinery {

    type Ref[A] = Prop.Ref[A]
    type Lang[K[_], A] = (TrackLang[Ref, ?[_], ?] :+: Prop.Lang  :++: Def.Lang )#Out[K, A]
    type State[K[_]]   = (Tracker[Ref, ?[_]]      :*: Prop.State :**: Def.State)#Out[K]

    private implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] = FreeKT.freeKTMonad[F, Id]

             val prgMonad: Monad[Prg] = freeKMonad[Lang]
    implicit val refEquality: HEqualK[Ref] = Prop.refEquality
    implicit val refShow: ShowK[Ref] = Prop.refShow

    implicit def freePropagation[F[_[_], _]](implicit i: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref] = Prop.freePropagation[F](i.compose[Prop.Lang])
    implicit def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], Cost] = Def.freeDeferApi[F](i.compose[Def.Lang](InjectK.injectRight(InjectK.injectRight)))
    implicit def freeTrackingApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Tracking[FreeK[F, ?], Ref] = TrackLang.freeTracking[Ref, F](i.compose[TrackLang[Ref, ?[_], ?]])

    def freeLib[F[_[_], _]](implicit i: InjectK[Lang, F]): Lib[FreeK[F, ?], Ref] = new Lib[FreeK[F, ?], Ref]

    val interpreter: StateInterpreter[Lang, State] = Tracker.interpreter[Ref] :&: Prop.interpreter :&&: Def.interpreter
    private val interpreterF = interpreter.freeInstance

    def empty[K[_]]: State[K] = Tracker.empty[Ref, K] :*: Prop.empty[K] :*: Def.empty[K]
    def fetch[K[_], D](ref: Ref[D], s: State[K]): D = Prop.fetch(ref, s._2._1)
    def interpret[A](prg: Prg[A], s: State[Prg]): (State[Prg], A) = interpreterF(prg)(s)

    val lib: Lib[Prg, Ref] =
      // all of the arguments are implicit, but scalac...
      new Lib[Prg, Ref]()(deferApi, propagationApi, trackingApi, prgMonad, refEquality.homogenize)
  }

  def newSession(): RefinerySession =
    newSession(refinery())(GoalKeeping.module)

  def newSession(r: Refinery)(g: GoalKeepingModule[r.Ref]): RefinerySession =
    new RefinerySessionImpl[r.State, g.State, r.Ref](r, g)
}
