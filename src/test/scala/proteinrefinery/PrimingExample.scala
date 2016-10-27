package proteinrefinery

import nutcracker.IncSet.IncSetRef
import nutcracker._
import nutcracker.util.ContU
import org.scalatest.FunSuite
import proteinrefinery.lib.syntax._
import proteinrefinery.lib.{AgentsPattern, Binding, PositiveInfluenceOnRule, Protein, ProteinModifications, ProteinPattern, Rule, SiteLabel}
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

  val watchForExplanationsViaPositiveInfluenceC: ContU[Prg, (Rule, PositiveInfluenceOnRule)] = for {
    ref1 <- Lib.forEachRule
    ref2 <- Lib.forEachRule
    r1 <- ref1.peekC[Prg]
    r2 <- ref2.peekC[Prg]
    (d1, r, d2) = r1.value unify r2.value
    diff <- if(d2.isEmpty && d1.isDefined) ContU.point[Prg, Rule.Delta](d1.get) else ContU.noop[Prg, Rule.Delta]
    diffPatterns = breakDown(r1.value.lhs, diff, r2.value.lhs)
    searches = diffPatterns.map(pp => Lib.positiveInfluenceOnRuleC(pp, r1.value))
    inflRef <- ContU.sequence(searches)
    infl <- inflRef.asCont[Prg]
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
