package proteinrefinery.util

final class Need1[Z, A] private(private[this] var eval: Z => A) {
  private var value: Option[A] = None

  def apply()(implicit z: Z): A = this.synchronized {
    value.getOrElse {
      val value0 = eval(z)
      value = Some(value0)
      eval = null
      value0
    }
  }
}

object Need1 {
  def apply[Z, A](eval: Z => A) = new Need1(eval)
}