package proteinrefinery

import nutcracker._
import org.scalatest.FunSuite
import proteinrefinery.capability.syntax._
import proteinrefinery.lib.{Assoc, Binding, CompetitiveBinding, NegativeInfluenceOnPhosphorylation, Nuggets, Phosphorylation, Protein, Site}

class Tests extends FunSuite {

  val bindings = List[Binding](
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

  val phosphoTargets = List[(Protein, Protein, Site)](
    ('C, 'B, 's)
  )

  val initialNuggets: Prg[Unit] = Nuggets.addAll(
    rules = bindings.map(_.witness),
    phosphoSites = phosphoTargets
  )

  val Interpreter = proteinrefinery.interpreterF

  test("Phosphorylation search") {
    val problem = Phosphorylation.search('C, 'B)
    val (s, ref) = Interpreter(initialNuggets >> problem)(proteinrefinery.emptyState)
    val solutionRefs = proteinrefinery.fetchIncSet(ref)(s)
    val solutions = solutionRefs.map(fetch(_)(s).value)
    val expected = Set(
      Phosphorylation(Assoc(List[Binding](bindings(1).flip, bindings(0))), Site("s")),              // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s"))
    )
    assertResult(expected)(solutions)
  }

  test("Negative influence on phosphorylation") {

    val problem = IncSet.collect(for {
      phosRef <- Phosphorylation.searchC('C, 'B)
      phos <- phosRef.asCont[DSL]
      niRef <- NegativeInfluenceOnPhosphorylation.searchC('D, phos)
      ni <- niRef.asCont[DSL]
    } yield (phos, ni))

    val (s, ref) = Interpreter(initialNuggets >> problem)(proteinrefinery.emptyState)
    val solutions = proteinrefinery.fetch(ref)(s).value

    val expected = Set[(Phosphorylation, NegativeInfluenceOnPhosphorylation)](
      (Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(3), bindings(4)))), // linter:ignore UseHeadNotApply
      (Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(8).flip, bindings(5))))
    )

    assertResult(expected)(solutions)
  }
}
