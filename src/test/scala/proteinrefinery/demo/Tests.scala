package proteinrefinery.demo

import nutcracker._
import nutcracker.util.{FreeK, Lst}
import org.scalatest.FunSuite
import proteinrefinery._
import proteinrefinery.capability.syntax._
import proteinrefinery.lib.{Assoc, Binding, CompetitiveBinding, KB, NegativeInfluenceOnAssociation, PhosSearch, Phosphorylation, Protein, ProteinModifications, Site}

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

  val TestKB = KB[Prg[Unit]](
    rules = bindings.map(_.witness),

    phosphoSites = List[(Protein, Protein, Site)](
      ('C, 'B, 's)
    )
  )

  val Interpreter = proteinrefinery.interpreterF

  test("Phosphorylation search") {
    val problem = PhosSearch.search('C, 'B)
    val (s, ref) = Interpreter(problem)(proteinrefinery.initialState(TestKB))
    val solutions = proteinrefinery.fetch(ref)(s).value
    val expected = Set(
      Phosphorylation(Assoc(List[Binding](bindings(1).flip, bindings(0))), Site("s")),
      Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")),
      Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s"))
    )
    assertResult(expected)(solutions)
  }

  test("Negative influence on phosphorylation") {

    val problem = IncSet.collect(for {
      phos <- PhosSearch.searchC('C, 'B)
      ni <- PhosSearch. negativeInfluenceC('D, phos)
    } yield (phos, ni))

    val (s, ref) = Interpreter(problem)(proteinrefinery.initialState(TestKB))
    val solutions = proteinrefinery.fetch(ref)(s).value

    val expected = Set[(Phosphorylation, NegativeInfluenceOnAssociation)](
      (Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), NegativeInfluenceOnAssociation.byCompetitiveBinding(CompetitiveBinding(bindings(3), bindings(4)))),
      (Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s")), NegativeInfluenceOnAssociation.byCompetitiveBinding(CompetitiveBinding(bindings(8).flip, bindings(5))))
    )

    assertResult(expected)(solutions)
  }
}
