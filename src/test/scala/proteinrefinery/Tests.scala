package proteinrefinery

import nutcracker._
import org.scalatest.FunSuite
import proteinrefinery.capability.syntax._
import proteinrefinery.lib.{Assoc, Binding, CompetitiveBinding, KB, NegativeInfluenceOnPhosphorylation, Nuggets, Phosphorylation, Protein, Site}

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

  val TestKB = KB[Prg[Unit]](
    rules = bindings.map(_.witness),

    phosphoSites = List[(Protein, Protein, Site)](
      ('C, 'B, 's)
    )
  )

  val initialNuggets: Prg2[Unit] = Nuggets.addAll(
    rules = bindings.map(_.witness),
    phosphoSites = phosphoTargets
  )

  val Interpreter = proteinrefinery.interpreterF
  val Interpreter2 = proteinrefinery.interpreter2F

  test("Phosphorylation search") {
    val problem = Phosphorylation.search('C, 'B)
    val (s, ref) = Interpreter(problem)(proteinrefinery.initialState(TestKB))
    val solutions = proteinrefinery.fetch(ref)(s).value
    val expected = Set(
      Phosphorylation(Assoc(List[Binding](bindings(1).flip, bindings(0))), Site("s")),              // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s"))
    )
    assertResult(expected)(solutions)
  }

  test("Phosphorylation search 2") {
    val problem = Phosphorylation.search_2('C, 'B)
    val (s, ref) = Interpreter2(initialNuggets >> problem)(proteinrefinery.emptyState2)
    val solutionRefs = proteinrefinery.fetchIncSet2(ref)(s)
    val solutions = solutionRefs.map(fetch2(_)(s).value)
    val expected = Set(
      Phosphorylation(Assoc(List[Binding](bindings(1).flip, bindings(0))), Site("s")),              // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), // linter:ignore UseHeadNotApply
      Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s"))
    )
    assertResult(expected)(solutions)
  }

  test("Negative influence on phosphorylation") {

    val problem = IncSet.collect(for {
      phos <- Phosphorylation.searchC('C, 'B)
      ni <- NegativeInfluenceOnPhosphorylation.searchC('D, phos)
    } yield (phos, ni))

    val (s, ref) = Interpreter(problem)(proteinrefinery.initialState(TestKB))
    val solutions = proteinrefinery.fetch(ref)(s).value

    val expected = Set[(Phosphorylation, NegativeInfluenceOnPhosphorylation)](
      (Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(3), bindings(4)))), // linter:ignore UseHeadNotApply
      (Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(8).flip, bindings(5))))
    )

    assertResult(expected)(solutions)
  }

  test("Negative influence on phosphorylation 2") {

    val problem = IncSet.collect(for {
      phosRef <- Phosphorylation.searchC_2('C, 'B)
      phos <- phosRef.asCont[DSL2]
      niRef <- NegativeInfluenceOnPhosphorylation.searchC_2('D, phos)
      ni <- niRef.asCont[DSL2]
    } yield (phos, ni))

    val (s, ref) = Interpreter2(initialNuggets >> problem)(proteinrefinery.emptyState2)
    val solutions = proteinrefinery.fetch2(ref)(s).value

    val expected = Set[(Phosphorylation, NegativeInfluenceOnPhosphorylation)](
      (Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(3), bindings(4)))), // linter:ignore UseHeadNotApply
      (Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s")), NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(CompetitiveBinding(bindings(8).flip, bindings(5))))
    )

    assertResult(expected)(solutions)
  }
}
