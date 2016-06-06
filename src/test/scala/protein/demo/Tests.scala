package protein.demo

import nutcracker._
import nutcracker.util.{FreeK, Lst}
import org.scalatest.FunSuite
import protein._
import protein.capability.syntax._
import protein.mechanism.{Assoc, Binding, CompetitiveBinding, Phosphorylation, Protein, ProteinModifications, Site}
import protein.search.PhosSearch

class Tests extends FunSuite {

  object TestKB extends KB[FreeK[Vocabulary, ?]] {

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

    def sitesOf(p: Protein)(f: Site => FreeK[Vocabulary, Unit]): (Lst[FreeK[Vocabulary, Unit]], KB[FreeK[Vocabulary, ?]], Unit) = {
      val sites = (bindings.iterator map (_.witness.mentionedSitesOf(p))).reduce(_ union _)
      (Lst.singleton(FreeK.traverse_(sites)(f)), this, ())
    }

    def phosphoSites(kinase: Protein, substrate: Protein)(f: Site => FreeK[Vocabulary, Unit]): (Lst[FreeK[Vocabulary, Unit]], KB[FreeK[Vocabulary, ?]], Unit) =
      (kinase, substrate) match {
        case (Protein('C), Protein('B)) => (Lst.singleton(f('s)), this, ())
        case _ => (Lst.empty, this, ())
      }

    def neighborsOf(p: Protein)(f: Binding => FreeK[Vocabulary, Unit]): (Lst[FreeK[Vocabulary, Unit]], KB[FreeK[Vocabulary, ?]], Unit) =
      (Lst.singleton(FreeK.traverse_(bindings.iterator.map(r => r.witness.linksAgentTo(p)).flatten.toSeq)(f)), this, ())

    def modsIncreasingKinaseActivity(kinase: Protein)(f: ProteinModifications => FreeK[Vocabulary, Unit]): (Lst[FreeK[Vocabulary, Unit]], KB[FreeK[Vocabulary, ?]], Unit) = ???
  }

  def bindings(i: Int) = TestKB.bindings(i)

  val Interpreter = protein.interpreter

  test("Phosphorylation search") {
    val problem = PhosSearch.search('C, 'B)
    val (s, ref) = Interpreter(problem)(protein.initialState(TestKB))
    val solutions = protein.fetch(ref)(s).value
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
      ni <- PhosSearch.negativeInfluenceC('D, phos)
    } yield (phos, ni))

    val (s, ref) = Interpreter(problem)(protein.initialState(TestKB))
    val solutions = protein.fetch(ref)(s).value

    val expected = Set[(Phosphorylation, CompetitiveBinding)](
      (Phosphorylation(Assoc(List[Binding](bindings(3), bindings(2).flip, bindings(0))), Site("s")), CompetitiveBinding(bindings(3), bindings(4))),
      (Phosphorylation(Assoc(List[Binding](bindings(8).flip, bindings(7), bindings(6).flip)), Site("s")), CompetitiveBinding(bindings(8).flip, bindings(5)))
    )

    assertResult(expected)(solutions)
  }
}
