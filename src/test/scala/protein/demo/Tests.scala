package protein.demo

import nutcracker.PropagationLang._
import org.scalatest.FunSuite
import protein._
import protein.capability.Binding
import protein.capability.syntax._
import protein.mechanism.{Assoc, CompetitiveBinding, Phosphorylation, Protein, ProteinModifications, Site}
import protein.search.PhosphorylationSearch

class Tests extends FunSuite {

  object TestKB extends KB {

    private val bindings = List[Binding](
      ('A @@ 'b) binds ('B @@ 'v),
      ('A('z~"p") @@ 'c) binds ('C @@ 'a),
      ('A @@ 'e) binds ('E @@ 'a),
      ('C @@ 'a) binds ('E @@ 'c),
      ('D @@ 'n) binds ('E @@ 'c),
      ('D @@ 'n) binds ('X @@ 'c),
      ('B @@ 'v) binds ('Y @@ 'b),
      ('X @@ 'y) binds ('Y @@ 'x),
      ('X @@ 'c) binds ('C @@ 'a)
    )

    def sitesOf(p: Protein): Seq[Site] = (
      (bindings.iterator filter { _.left.p.p == p } map { _.left.s }).toSet ++
        (bindings.iterator filter { _.right.p.p == p } map { _.right.s })
      ).toSeq

    def phosphoSites(kinase: Protein, substrate: Protein): Seq[Site] = (kinase, substrate) match {
      case (Protein('C), Protein('B)) => Seq('s)
      case _ => Seq()
    }

    def neighborsOf(p: Protein): Seq[Binding] =
      (bindings filter { _.left.p.p == p  }) ++
        (bindings filter { _.right.p.p == p } map { _.flip })

    def modsIncreasingKinaseActivity(kinase: Protein): Seq[ProteinModifications] = Seq()
  }

  val Solver = protein.bfsSolver

  test("Phosphorylation search") {
    val problem = PhosphorylationSearch.search('C, 'B)
    val solutions = Solver.solutions(problem).toStream.run(TestKB).toSet
    val expected = Set(
      (Phosphorylation(Assoc(List[Binding](('C @@ 'a) binds ('A('z~"p") @@ 'c), ('A @@ 'b) binds ('B @@ 'v))), Site("s")), Cost.complexity(10)),
      (Phosphorylation(Assoc(List[Binding](('C @@ 'a) binds ('E @@ 'c), ('E @@ 'a) binds ('A @@ 'e), ('A @@ 'b) binds ('B @@ 'v))), Site("s")), Cost.complexity(20)),
      (Phosphorylation(Assoc(List[Binding](('C @@ 'a) binds ('X @@ 'c), ('X @@ 'y) binds ('Y @@ 'x), ('Y @@ 'b) binds ('B @@ 'v))), Site("s")), Cost.complexity(20))
    )
    assertResult(expected)(solutions)
  }

  test("Negative influence on phosphorylation") {

    val problem = for {
      phos0 <- PhosphorylationSearch.search0('C, 'B)
      ni <- PhosphorylationSearch.negativeInfluence('D, phos0)
      phos <- PhosphorylationSearch.fetch(phos0)
      pr <- promiseC[Vocabulary].tuple(phos, ni)
    } yield pr

    val solutions = Solver.solutions(problem).toStream.run(TestKB).toSet

    val expected = Set[((Phosphorylation, CompetitiveBinding), Cost)](
      ((Phosphorylation(Assoc(List[Binding](('C @@ 'a) binds ('E @@ 'c), ('E @@ 'a) binds ('A @@ 'e), ('A @@ 'b) binds ('B @@ 'v))), Site("s")), CompetitiveBinding(('C @@ 'a) binds ('E @@ 'c), ('D @@ 'n))), Cost.complexity(20)),
      ((Phosphorylation(Assoc(List[Binding](('C @@ 'a) binds ('X @@ 'c), ('X @@ 'y) binds ('Y @@ 'x), ('Y @@ 'b) binds ('B @@ 'v))), Site("s")), CompetitiveBinding(('C @@ 'a) binds ('X @@ 'c), ('D @@ 'n))), Cost.complexity(20))
    )

    assertResult(expected)(solutions)
  }
}
