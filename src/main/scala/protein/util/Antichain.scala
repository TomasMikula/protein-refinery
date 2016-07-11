package protein.util

import nutcracker.{DRef, Dom}
import nutcracker.Dom.{Refined, Status}

final case class Antichain[A](value: A) extends AnyVal

object Antichain {
  type Update[A] = Nothing
  type Delta[A] = Nothing

  type Ref[A] = DRef.Aux[Antichain[A], Update[A], Delta[A]]

  trait DomType[A] extends protein.DomType[A] { self: Singleton =>
    override type Update = Nothing
    override type Delta = Nothing
  }

  implicit def domInstance[A]: Dom.Aux[Antichain[A], Update[A], Delta[A]] = new Dom[Antichain[A]] {
    type Update = Antichain.Update[A]
    type Delta = Antichain.Delta[A]

    def update(d: Antichain[A], u: Update): Option[(Antichain[A], Delta)] = None
    def combineDeltas(d1: Delta, d2: Delta): Delta = sys.error("unreachable code")
    def assess(d: Antichain[A]): Status[Update] = Refined
  }

}