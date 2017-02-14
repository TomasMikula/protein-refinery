import scala.language.higherKinds
import monocle.Lens
import nutcracker._
import nutcracker.Propagation.{module => Prop}
import nutcracker.util.{FreeK, HEqualK, ShowK}
import nutcracker.util.CoproductK._
import nutcracker.util.KList._
import proteinrefinery.util.{TrackLang, Tracker}

import scalaz.~>
import scalaz.Id._

package object proteinrefinery extends ImplicitConversions {
  import Prop._

  type DeferL[K[_], A] = DeferLang[Cost, K, A]
  type DeferS[K] = DeferStore[Cost, K]

  type DSL[K[_], A] = (TrackLang[Ref, ?[_], ?] :+: Prop.Lang  :++: DeferL)#Out[K, A]
  type State[K]     = (Tracker[Ref, ?]         :*: Prop.State :**: DeferS)#Out[K]
  type Prg[A] = FreeK[DSL, A]
  private[proteinrefinery] type PU = Prg[Unit]

  val Lib = new Lib[Prg, Ref]

  val interpreter = Tracker.interpreter[Ref] :&: Prop.interpreter :&&: DeferStore.interpreter[Cost]
  val interpreterF = interpreter.freeInstance
  def propStore[K]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  def fetch[D](ref: Ref[D])(s: State[PU]): D = Prop.fetch(propStore[PU].get(s))(ref)
  def fetchIncSet[D](ref: Ref[IncSet[D]])(s: State[PU]): Set[D] = {
    fetch(ref)(s).value
  }
  def initialState[K](tr: Tracker[Ref, K]): State[K] = tr :*: empty[K] :**: (DeferStore.empty[Cost, K]: DeferS[K])
  def emptyState[K]: State[K] = initialState(Tracker.empty[Ref, K])

  def refinery(): Refinery { type M[A] = Prg[A]; type Ref[A] = Prop.Ref[A] } = new Refinery {
    type M[A] = Prg[A]
    type Ref[A] = Prop.Ref[A]

    private var state: State[PU] = emptyState

    implicit val refEquality: HEqualK[Ref] = Prop.refEquality
    implicit val refShow: ShowK[Ref] = Prop.refShow

    implicit val fetch: Ref ~> Id =
      λ[Ref ~> Id](proteinrefinery.fetch(_)(state))

    def interpret[A](prg: Prg[A]): A = {
      val (s, a) = interpreterF(prg)(state)
      state = s
      a
    }

    val lib: Lib[Prg, Ref] = Lib
  }
}
