package proteinrefinery

import nutcracker.util.EqualK
import proteinrefinery.lib.PhosphoTriple
import scala.language.higherKinds
import scalaz.syntax.monad._

class Tests extends TestSuite {

  def bindings[Ref[_]](implicit ev: EqualK[Ref]): List[lib.BindingData[Ref]] = {
    val syntax = lib.Syntax[Ref]
    import syntax._

    List[lib.BindingData[Ref]](
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
  }

  def phosTargets[Ref[_]]: List[PhosphoTriple[Ref]] = {
    val syntax = lib.Syntax[Ref]
    import syntax._

    List[PhosphoTriple[Ref]](
      'C phosphorylates 'B at 's
    )
  }

  def initPhospho(implicit s: RefinerySession): s.Prg[Unit] = {
    s.lib.addNuggets(
      phosphoSites = phosTargets
    )
  }

  def initBindings(implicit s: RefinerySession): s.Prg[List[s.lib.Binding]] = {
    import s.lib._

    addBindings(bindings[s.Ref](RefEquality))
  }

  test("Phosphorylation search") {
    implicit val session = proteinrefinery.newSession()
    import session.{lib => _, _}
    import session.lib._

    val problem = phosphorylations('C, 'B)

    val bnds = interpret(initPhospho >> initBindings)

    val solutions = fetch(interpret(problem))

    assertResult(1)(solutions.size)

    val pt = fetch(solutions.head).value

    assertEqual(Protein('C))(pt.kinase)
    assertEqual(Protein('B))(pt.substrate)
    assertEqual(ISite('s))(pt.targetSite)

    val assocss = pt.witness.lhs.assocs

    assertResult(1)(assocss.size)

    val Some((i, j, asRef)) = assocss.head
    val assocs = fetch(asRef).value.map(aRef => fetch(aRef).value)

    assertDeepEqual(Set(
      Assoc(List(bnds(1).flip, bnds(0))),          // linter:ignore UseHeadNotApply
      Assoc(List(bnds(3), bnds(2).flip, bnds(0))), // linter:ignore UseHeadNotApply
      Assoc(List(bnds(8).flip, bnds(7), bnds(6).flip))
    ))(assocs)
  }

  test("Negative influence on phosphorylation") {
    implicit val session = proteinrefinery.newSession()
    import session.{lib => _, _}
    import session.lib._

    val problem = for {
      phosRef <- phosphorylations('C, 'B)
      niRef <- IncSets.relBind(phosRef)(negativeInfluenceOnPhosphorylation_r('D, _))
    } yield (phosRef, niRef)

    val bnds = interpret(initPhospho >> initBindings)
    val (phossRef, nisRef) = interpret(problem)

    val phoss = fetch(phossRef)
    assert(phoss.size == 1)

    val phosRef = phoss.value.head
    val phos = fetch(phosRef).value

    assertEqual(Protein('C))(phos.kinase)
    assertEqual(Protein('B))(phos.substrate)
    assertEqual(ISite('s))(phos.targetSite)

    val paths = phos.witness.lhs.assocs
    assert(paths.size == 1)

    val assocsRef = paths(0).get._3
    val assocs = fetch(assocsRef)

    assert(assocs.size == 3) // same 3 as in the previous test

    val nis = fetch(nisRef).value.map(niRef => fetch(niRef).value)

    assertDeepEqual(Set(
      lib.NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(lib.CompetitiveBinding(bnds(3), bnds(4))),
      lib.NegativeInfluenceOnPhosphorylation.byCompetitiveBinding(lib.CompetitiveBinding(bnds(8).flip, bnds(5)))
    ))(nis)
  }
}
