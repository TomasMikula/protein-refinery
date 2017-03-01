import scala.language.higherKinds
import nutcracker._
import nutcracker.Propagation.{module => Prop}
import nutcracker.util.{FreeK, FreeKT, HEqualK, InjectK, ShowK}
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._
import proteinrefinery.util.{TrackLang, Tracker, Tracking}
import scalaz.Monad

package object proteinrefinery extends ImplicitConversions {
  import Prop.Ref
  val Def = DeferModule.instance[Cost]

  type DSL[K[_], A] = (TrackLang[Ref, ?[_], ?] :+: Prop.Lang  :++: Def.Lang )#Out[K, A]
  type State[K[_]]  = (Tracker[Ref, ?[_]]      :*: Prop.State :**: Def.State)#Out[K]
  type Prg[A] = FreeK[DSL, A]

  val interpreter = Tracker.interpreter[Ref] :&: Prop.interpreter :&&: Def.interpreter
  val interpreterF = interpreter.freeInstance
  def initialState[K[_]](tr: Tracker[Ref, K]): State[K] = tr :*: Prop.empty[K] :*: Def.empty[K]
  def emptyState[K[_]]: State[K] = initialState(Tracker.empty[Ref, K])

  def refinery(): Refinery = new Refinery {
    type Prg[A] = proteinrefinery.Prg[A]
    type Ref[A] = Prop.Ref[A]

    implicit val prgMonad: Monad[Prg] = FreeKT.freeKTMonad
    implicit val refEquality: HEqualK[Ref] = Prop.refEquality
    implicit val refShow: ShowK[Ref] = Prop.refShow
    implicit val propagationApi: Propagation[Prg, Ref] = Prop.freePropagation[DSL]
    implicit val trackingApi: Tracking[Prg, Ref] = TrackLang.freeTracking[Ref, DSL]
    implicit val deferApi: Defer[Prg, Cost] = Def.freeDeferApi[DSL](InjectK.injectRight(InjectK.injectRight)) // should not be necessary after https://issues.scala-lang.org/browse/SI-10213

    def fetch[D](ref: Ref[D])(s: State[Prg]): D = Prop.fetch(s._2._1)(ref)
    def interpret[A](prg: Prg[A], s: State[Prg]): (State[Prg], A) = interpreterF(prg)(s)

    val lib: Lib[Prg, Ref] = new Lib[Prg, Ref]
  }

  def newSession(): RefinerySession =
    newSession(refinery())

  def newSession(r: Refinery): RefinerySession.Aux[r.Prg, r.Ref] =
    new RefinerySessionImpl[r.Prg, r.Ref](r)
}
