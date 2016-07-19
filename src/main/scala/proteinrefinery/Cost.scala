package proteinrefinery

import nutcracker.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}

import scalaz.Ordering

case class Cost(complexity: Long, speculation: Long, vagueness: Long) {

  override def toString = s"complexity: $complexity, speculation: $speculation, vagueness: $vagueness"

}

object Cost {
  def complexity(x: Long) = Cost(x, 0, 0)
  def speculation(x: Long) = Cost(0, x, 0)
  def vagueness(x: Long) = Cost(0, 0, x)

  implicit val orderedMonoid: NonDecreasingMonoid[Cost] with OrderPreservingMonoid[Cost] =
    new NonDecreasingMonoid[Cost] with OrderPreservingMonoid[Cost] {
      def zero: Cost = Cost(0, 0, 0)

      def append(c1: Cost, c2: => Cost): Cost = Cost(
        c1.complexity + c2.complexity,
        c1.speculation + c2.speculation,
        c1.vagueness + c2.vagueness
      )

      def order(x: Cost, y: Cost): Ordering = {
        import Ordering._
        val sx = x.complexity + x.speculation + x.vagueness
        val sy = y.complexity + y.speculation + y.vagueness
        if(sx < sy) LT else if (sx == sy) EQ else GT
      }
    }
}