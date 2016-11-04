package proteinrefinery.util

import scala.language.higherKinds

trait ShowK[F[_]] {
  def shows[A](fa: F[A]): String
}
