package proteinrefinery

import nutcracker.util.{Desc, ShowK}
import org.scalatest.FunSuite
import scalaz.syntax.monad._

class PositiveInfluenceOnRuleTest extends FunSuite {
  val refinery = proteinrefinery.refinery()
  import refinery._
  import refinery.lib._

  val β_TrCP = Protein("β-TrCP")
  val β_Cat  = Protein("β-Catenin")
  val CK1    = Protein("CK1")
  val GSK    = Protein("GSK")

  val bindings: List[BindingData] = List[BindingData](
    /* 00 */ (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p") @@ 'y),
    /* 01 */ (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p", 'T41~"p", 'S45~"p") @@ 'y),
    /* 02 */ (CK1 @@ 'a) binds (β_Cat @@ 'b),
    /* 03 */ (GSK @@ 'a) binds (β_Cat('S45~"p") @@ 'c),
    /* 04 */ (GSK @@ 'a) binds (β_Cat('T41~"p") @@ 'd),
    /* 05 */ (GSK @@ 'a) binds (β_Cat('S37~"p") @@ 'e)
  )

  val phosphoTargets = List[PhosphoTriple](
    CK1 phosphorylates β_Cat at 'S45,
    GSK phosphorylates β_Cat at 'T41,
    GSK phosphorylates β_Cat at 'S37,
    GSK phosphorylates β_Cat at 'S33
  )

  val initBindings: Prg[List[Binding]] = addBindings(bindings)

  val initPhospho: Prg[Unit] = addNuggets(phosphoSites = phosphoTargets)

  test("1") {
    val session = newSession(refinery)

    val bnds = session.interpret(initPhospho >> initBindings)
    val solutions = session.interpretFetch(positiveInfluenceOnRule(β_Cat('S45~"p", 'T41~"p"), bnds(3).witness))
    assertResult(1)(solutions.size)
    val sol = solutions.head
    assertResult(PositiveInfluenceOnRule.InLhs(β_Cat('S45~"p", 'T41~"p"), bnds(3).witness))(sol)
  }

  test("2") {
    val session = newSession(refinery)

    val bnds = session.interpret(initPhospho >> initBindings)
    val solutions = session.interpretFetch(positiveInfluenceOnRule(β_Cat('S45~"p", 'T41~"p"), bnds(4).witness))
    assertResult(1, Desc(solutions).printTree(session.deref, implicitly[ShowK[refinery.Ref]], lineLimit = 80)())(solutions.size)
    val sol = solutions.head
    assertResult(PositiveInfluenceOnRule.InLhs(β_Cat('S45~"p", 'T41~"p"), bnds(4).witness))(sol)
  }

  test("3") {
    val session = newSession(refinery)

    val bnds = session.interpret(initPhospho >> initBindings)
    val solutions = session.interpretFetch(positiveInfluenceOnRule(β_Cat('S45~"p", 'T41~"p"), bnds(5).witness))
    assertResult(1, Desc(solutions).printTree(session.deref, implicitly[ShowK[refinery.Ref]], lineLimit = 80)())(solutions.size)
    val sol = solutions.head
    assertResult(???)(sol)
  }
}
