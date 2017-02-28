import scala.language.higherKinds
import nutcracker._
import nutcracker.Propagation.{module => Prop}
import nutcracker.util.{FreeK, FreeKT, HEqualK, ShowK}
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._
import proteinrefinery.util.{TrackLang, Tracker}
import scalaz.{Monad}

package object proteinrefinery extends ImplicitConversions {
  import Prop._

  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K[_]]    = DeferStore[Cost, K]

  type DSL[K[_], A] = (TrackLang[Ref, ?[_], ?] :+: Prop.Lang  :++: DeferL)#Out[K, A]
  type State[K[_]]  = (Tracker[Ref, ?[_]]      :*: Prop.State :**: DeferS)#Out[K]
  type Prg[A] = FreeK[DSL, A]

  val interpreter = Tracker.interpreter[Ref] :&: Prop.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreterF = interpreter.freeInstance
  def initialState[K[_]](tr: Tracker[Ref, K]): State[K] = tr :*: empty[K] :*: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState[K[_]]: State[K] = initialState(Tracker.empty[Ref, K])

  def refinery(): Refinery = new Refinery {
    type Prg[A] = proteinrefinery.Prg[A]
    type Ref[A] = Prop.Ref[A]

    implicit val prgMonad: Monad[Prg] = FreeKT.freeKTMonad
    implicit val refEquality: HEqualK[Ref] = Prop.refEquality
    implicit val refShow: ShowK[Ref] = Prop.refShow

    def fetch[D](ref: Ref[D])(s: State[Prg]): D = Prop.fetch(s._2._1)(ref)
    def interpret[A](prg: Prg[A], s: State[Prg]): (State[Prg], A) = interpreterF(prg)(s)

    val lib: Lib[Prg, Ref] = new Lib[Prg, Ref]
  }

  def newSession(): RefinerySession = newSession(refinery())

  def newSession(r: Refinery): RefinerySession.Aux[r.Prg, r.Ref] = new RefinerySession {
    val refinery: Refinery.Aux[r.Prg, r.Ref] = r

    type Prg[A] = r.Prg[A]
    type Ref[A] = r.Ref[A]

    implicit val prgMonad: Monad[Prg] = refinery.prgMonad
    implicit val refEquality: HEqualK[Ref] = refinery.refEquality
    implicit val refShow: ShowK[Ref] = refinery.refShow

    var state: State[Prg] = emptyState

    def fetch[A](ref: Ref[A]): A =
      refinery.fetch(ref)(state)

    def interpret[A](prg: Prg[A]): A = {
      val (s, a) = refinery.interpret(prg, state)
      state = s
      a
    }

    val lib: Lib[Prg, Ref] = refinery.lib
  }
}
