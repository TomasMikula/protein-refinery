package proteinrefinery

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.ContF
import org.scalatest.FunSuite
import proteinrefinery.lib.syntax._
import proteinrefinery.lib.{AgentsPattern, Binding, PositiveInfluenceOnRule, Protein, ProteinModifications, ProteinPattern, Rule, Lib, SiteLabel}
import proteinrefinery.util.Unification.Syntax._

class PrimingExample extends FunSuite {

  val β_TrCP = Protein("β-TrCP")
  val β_Cat  = Protein("β-Catenin")
  val CK1    = Protein("CK1")
  val GSK    = Protein("GSK")

  val bindings: List[Binding] = List[Binding](
    (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p") @@ 'y),
    (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p", 'T41~"p", 'S45~"p") @@ 'y),
    (CK1 @@ 'a) binds (β_Cat @@ 'b),
    (GSK @@ 'a) binds (β_Cat('S45~"p") @@ 'c),
    (GSK @@ 'a) binds (β_Cat('T41~"p") @@ 'd),
    (GSK @@ 'a) binds (β_Cat('S37~"p") @@ 'e)
  )

  val phosphoTargets = List[(Protein, Protein, SiteLabel)](
    (CK1, β_Cat, 'S45),
    (GSK, β_Cat, 'T41),
    (GSK, β_Cat, 'S37),
    (GSK, β_Cat, 'S33)
  )

  val initialNuggets: Prg[Unit] = Lib.addNuggets(
    rules = bindings.map(_.witness),
    phosphoSites = phosphoTargets
  )

  private def forEachRule = Lib.forEachRule[DSL]
  private def peekC[D](ref: DRef[D]): ContF[DSL, D] = ContF(f => PropagationLang.peek(ref)(f))

  val watchForExplanationsViaPositiveInfluenceC: ContF[DSL, (Rule, PositiveInfluenceOnRule)] = for {
    ref1 <- forEachRule
    ref2 <- forEachRule
    r1 <- peekC(ref1)
    r2 <- peekC(ref2)
    (d1, r, d2) = r1.value unify r2.value
    diff <- if(d2.isEmpty && d1.isDefined) ContF.point[DSL, Rule.Delta](d1.get) else ContF.noop[DSL, Rule.Delta]
    diffPatterns = breakDown(r1.value.lhs, diff, r2.value.lhs)
    searches = diffPatterns.map(pp => Lib.positiveInfluenceOnRuleC(pp, r1.value))
    inflRef <- ContF.sequence(searches)
    infl <- inflRef.asCont[DSL]
  } yield (r2.value, infl)

  val watchForExplanationsViaPositiveInfluence: Prg[IncSetRef[(Rule, PositiveInfluenceOnRule)]] =
    IncSet.collect(watchForExplanationsViaPositiveInfluenceC)

  val Interpreter = proteinrefinery.interpreterF

  test("suspicion search") {
    val program = initialNuggets >> watchForExplanationsViaPositiveInfluence
    val (s, ref) = Interpreter(program)(proteinrefinery.emptyState)
    val solutions = proteinrefinery.fetchIncSet(ref)(s)
    assert(solutions.nonEmpty)
    assertResult(???)(solutions)
  }

  private def breakDown(ap1: AgentsPattern, d: AgentsPattern.Delta, ap2: AgentsPattern): List[ProteinPattern] = { // linter:ignore UnusedParameter
    d.agentDeltas.toList flatMap { case (i, ppd) =>
      val p = ap2(i).protein
      if(p.isValid) {
        ppd.b.toList.flatMap(pmd => pmd.newElements.filter(_.state.nonEmpty)) match {
          case Nil => Nil
          case sss => List(ProteinPattern(p, ProteinModifications(sss)))
        }
      } else {
        Nil
      }
    }
  }
}
