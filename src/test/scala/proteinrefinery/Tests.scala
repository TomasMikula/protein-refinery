package proteinrefinery

import nutcracker._
import org.scalatest.FunSuite
import proteinrefinery.lib.syntax._
import proteinrefinery.lib.{Assoc, Binding, CompetitiveBinding, NegativeInfluenceOnPhosphorylation, Phosphorylation, Protein, SiteLabel}

class Tests extends FunSuite {

  val bindings: List[Binding] = List[Binding](
    /* 00 */ ('A @@ 'b) binds ('B @@ 'v),
    /* 01 */ ('A('z~"p") @@ 'c) binds ('C @@ 'a),
    /* 02 */ ('A @@ 'e) binds ('E @@ 'a),
    /* 03 */ ('C @@ 'a) binds ('E @@ 'c),
    /* 04 */ ('D @@ 'n) binds ('E @@ 'c),
    /* 05 */ ('D @@ 'n) binds ('X @@ 'c),
    /* 06 */ ('B @@ 'v) binds ('Y @@ 'b),
    /* 07 */ ('X @@ 'y) binds ('Y @@ 'x),
    /* 08 */ ('X @@ 'c) binds ('C @@ 'a)
  )

  val phosphoTargets = List[(Protein, Protein, SiteLabel)](
    ('C, 'B, 's)
  )

  val initialNuggets: Prg[Unit] = Lib.addNuggets(
    rules = bindings.map(_.witness),
    phosphoSites = phosphoTargets
  )

  val Interpreter = proteinrefinery.interpreterF

  test("Phosphorylation search") {
    val problem = Lib.phosphorylation('C, 'B)
    val (s, ref) = Interpreter(initialNuggets >> problem)(proteinrefinery.emptyState)
    val solutionRefs = proteinrefinery.fetchIncSet(ref)(s)
    val solutions = solutionRefs.map(fetch(_)(s).value)
    val expected = Set(
      Phosphorylation(Assoc(List[Binding](bindings(1).flip, bindings(0))), SiteLabel("s")),              // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), SiteLabel("s")), // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), SiteLabel("s"))
    )
    assertResult(expected)(solutions)
  }

  test("Negative influence on phosphorylation") {

    val problem = IncSet.collect(for {
      phosRef <- Lib.phosphorylationC('C, 'B)
      phos <- phosRef.asCont[Prg]
      niRef <- Lib .negativeInfluenceOnPhosphorylationC('D, phos)
      ni <- niRef.asCont[Prg]
    } yield (phos, ni))

    val (s, ref) = Interpreter(initialNuggets >> problem)(proteinrefinery.emptyState)
    val solutions = proteinrefinery.fetch(ref)(s).value

    val expected = Set[(Phosphorylation, NegativeInfluenceOnPhosphorylation)](
      (Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), SiteLabel("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(3), bindings(4)))), // linter:ignore UseHeadNotApply
      (Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), SiteLabel("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(8).flip, bindings(5))))
    )

    assertResult(expected)(solutions)
  }
}
