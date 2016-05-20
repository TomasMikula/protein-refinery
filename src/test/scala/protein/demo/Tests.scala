package protein.demo

import nutcracker._
import org.scalatest.FunSuite
import protein._
import protein.capability.syntax._
import protein.mechanism.{Assoc, Binding, CompetitiveBinding, Phosphorylation, Protein, ProteinModifications, Site}
import protein.search.PhosphorylationSearch

class Tests extends FunSuite {

  object TestKB extends KB {

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

    def sitesOf(p: Protein): Seq[Site] =
      (bindings.iterator map (_.witness.mentionedSitesOf(p))).reduce(_ union _).toSeq

    def phosphoSites(kinase: Protein, substrate: Protein): Seq[Site] = (kinase, substrate) match {
      case (Protein('C), Protein('B)) => Seq('s)
      case _ => Seq()
    }

    def neighborsOf(p: Protein): Seq[Binding] =
      bindings.iterator.map(r => r.witness.linksAgentTo(p)).flatten.toSeq

    def modsIncreasingKinaseActivity(kinase: Protein): Seq[ProteinModifications] = Seq()
  }

  def bindings(i: Int) = TestKB.bindings(i)

  val Solver = protein.bfsSolver

  test("Phosphorylation search") {
    val problem = PhosphorylationSearch.search('C, 'B)
    val solutions = Solver.solutions(problem).toStream.run(TestKB).toSet
    val expected = Set(
      (Phosphorylation(Assoc(List[Binding](bindings(1).flip, bindings(0))), Site("s")), Cost.complexity(10)),
      (Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), Cost.complexity(20)),
      (Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s")), Cost.complexity(20))
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
      ((Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), CompetitiveBinding(bindings(3), bindings(4))), Cost.complexity(20)),
      ((Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s")), CompetitiveBinding(bindings(8).flip, bindings(5))), Cost.complexity(20))
    )

    assertResult(expected)(solutions)
  }
}
