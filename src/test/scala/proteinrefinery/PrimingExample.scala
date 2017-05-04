package proteinrefinery

import nutcracker.data.IncSet
import nutcracker.ops._
import nutcracker.util.ContU
import org.scalatest.FunSuite
import proteinrefinery.util.Unification.Syntax._

class PrimingExample extends FunSuite {
  val refinery = proteinrefinery.refinery()
  import refinery._
  import refinery.lib._

  val β_TrCP = Protein("β-TrCP")
  val β_Cat  = Protein("β-Catenin")
  val CK1    = Protein("CK1")
  val GSK    = Protein("GSK")

  val bindings: List[BindingData] = List[BindingData](
    (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p") @@ 'y),
    (β_TrCP @@ 'x) binds (β_Cat('S33~"p", 'S37~"p", 'T41~"p", 'S45~"p") @@ 'y),
    (CK1 @@ 'a) binds (β_Cat @@ 'b),
    (GSK @@ 'a) binds (β_Cat('S45~"p") @@ 'c),
    (GSK @@ 'a) binds (β_Cat('T41~"p") @@ 'd),
    (GSK @@ 'a) binds (β_Cat('S37~"p") @@ 'e)
  )

  val phosphoTargets = List[PhosphoTriple](
    CK1 phosphorylates β_Cat at 'S45,
    GSK phosphorylates β_Cat at 'T41,
    GSK phosphorylates β_Cat at 'S37,
    GSK phosphorylates β_Cat at 'S33
  )

  val initialNuggets: Prg[Unit] = addNuggets(
    rules = bindings.map(_.witness),
    phosphoSites = phosphoTargets
  )

  val watchForExplanationsViaPositiveInfluenceC: ContU[Prg, (Rule, PositiveInfluenceOnRule)] = for {
    ref1 <- lib.forEachRule
    ref2 <- lib.forEachRule
    r1 <- ref1.peekC
    r2 <- ref2.peekC
    (d1, r, d2) = r1.value unify r2.value
    diff <- if(d2.isEmpty && d1.isDefined) ContU.point[Prg, Rule.Delta](d1.get) else ContU.noop[Prg, Rule.Delta]
    diffPatterns = breakDown(r1.value.lhs, diff, r2.value.lhs)
    searches = diffPatterns.map(pp => positiveInfluenceOnRuleC(pp, ref1))
    infl <- ContU.sequence(searches)
  } yield (r2.value, infl)

  val watchForExplanationsViaPositiveInfluence: Prg[Var[IncSet[(Rule, PositiveInfluenceOnRule)]]] =
    IncSets.collect(watchForExplanationsViaPositiveInfluenceC)

  test("suspicion search") {
    val program = initialNuggets >> watchForExplanationsViaPositiveInfluence
    val solutions = eval(for {
      ref <- interpret(program)
      solutions <- fetch(ref)
    } yield solutions.value)
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
