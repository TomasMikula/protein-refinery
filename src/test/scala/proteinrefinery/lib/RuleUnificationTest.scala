package proteinrefinery.lib

import org.scalatest.FunSuite
import proteinrefinery.lib.syntax._
import proteinrefinery.util.Unification.Syntax._

class RuleUnificationTest extends FunSuite {

  test("unification") {
    val β_TrCP = Protein("β-TrCP")
    val β_Cat  = Protein("β-Catenin")

    val b1 = (β_TrCP @@ 'x) binds (β_Cat('S33 ~ "p", 'S37 ~ "p") @@ 'y)
    val b2 = (β_TrCP @@ 'x) binds (β_Cat('S33 ~ "p", 'S37 ~ "p", 'T41 ~ "p", 'S45 ~ "p") @@ 'y)

    val r1 = b1.witness
    val r2 = b2.witness

    val (d1, r, d2) = r1 unify r2

    assert(d1.isDefined)
    assert(d2.isEmpty)
    assertResult(r2)(r)
  }
}
