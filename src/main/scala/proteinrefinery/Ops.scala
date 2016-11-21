package proteinrefinery

import scala.language.implicitConversions

import scalaz.{Lens, Store}

trait Ops {
  import Ops._

  implicit def lensOps[S](s: S): LensOps[S] = LensOps(s)
}

object Ops {
  case class LensOps[S](s: S) extends AnyVal {
    def focus[A](lens: Lens[S, A]): Store[A, S] = lens(s)
  }
}