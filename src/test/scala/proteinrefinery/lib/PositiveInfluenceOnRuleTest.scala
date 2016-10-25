package proteinrefinery.lib

import org.scalatest.FunSuite
import proteinrefinery._
import proteinrefinery.lib.syntax._

class PositiveInfluenceOnRuleTest extends FunSuite {

  val β_TrCP = Protein("β-TrCP")
  val β_Cat  = Protein("β-Catenin")
  val CK1    = Protein("CK1")
  val GSK    = Protein("GSK")

  val bindings: List[Binding] = List[Binding](
    /* 00 */ (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p") @@ 'y),
    /* 01 */ (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p", 'T41~"p", 'S45~"p") @@ 'y),
    /* 02 */ (CK1 @@ 'a) binds (β_Cat @@ 'b),
    /* 03 */ (GSK @@ 'a) binds (β_Cat('S45~"p") @@ 'c),
    /* 04 */ (GSK @@ 'a) binds (β_Cat('T41~"p") @@ 'd),
    /* 05 */ (GSK @@ 'a) binds (β_Cat('S37~"p") @@ 'e)
  )

  val phosphoTargets = List[(Protein, Protein, SiteLabel)](
    (CK1, β_Cat, 'S45),
    (GSK, β_Cat, 'T41),
    (GSK, β_Cat, 'S37),
    (GSK, β_Cat, 'S33)
  )

  val initialNuggets: Prg[Unit] = Nuggets.addAll(
    rules = bindings.map(_.witness),
    phosphoSites = phosphoTargets
  )

  val Interpreter = proteinrefinery.interpreterF

  test("1") {
    val prg = initialNuggets >> PositiveInfluenceOnRule.search(β_Cat('S45~"p", 'T41~"p"), bindings(3).witness)
    val (s, solutionsRef) = Interpreter(prg)(proteinrefinery.emptyState)
    val solutionRefs = proteinrefinery.fetchIncSet(solutionsRef)(s)
    val solutions = solutionRefs.toList.map(ref => proteinrefinery.fetch(ref)(s))
    assertResult(1)(solutions.size)
    val sol = solutions.head.value
    assertResult(PositiveInfluenceOnRule.InLhs(β_Cat('S45~"p", 'T41~"p"), bindings(3).witness))(sol)
  }

  test("2") {
    val prg = initialNuggets >> PositiveInfluenceOnRule.search(β_Cat('S45~"p", 'T41~"p"), bindings(4).witness)
    val (s, solutionsRef) = Interpreter(prg)(proteinrefinery.emptyState)
    val solutionRefs = proteinrefinery.fetchIncSet(solutionsRef)(s)
    val solutions = solutionRefs.toList.map(ref => proteinrefinery.fetch(ref)(s))
    assertResult(1)(solutions.size)
    val sol = solutions.head.value
    assertResult(PositiveInfluenceOnRule.InLhs(β_Cat('S45~"p", 'T41~"p"), bindings(4).witness))(sol)
  }

  test("3") {
    val prg = initialNuggets >> PositiveInfluenceOnRule.search(β_Cat('S45~"p", 'T41~"p"), bindings(5).witness)
    val (s, solutionsRef) = Interpreter(prg)(proteinrefinery.emptyState)
    val solutionRefs = proteinrefinery.fetchIncSet(solutionsRef)(s)
    val solutions = solutionRefs.toList.map(ref => proteinrefinery.fetch(ref)(s))
    assertResult(1)(solutions.size)
    val sol = solutions.head.value
    assertResult(???)(sol)
  }
}
