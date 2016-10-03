package proteinrefinery.lib

import org.scalatest.{Assertion, FunSuite}
import proteinrefinery.lib.AdmissibleProteinModifications.SiteWithState
import proteinrefinery.lib.AdmissibleProteinModifications.SiteWithState._

import scalaz.Equal

class ProteinModificationsTests extends FunSuite {

  private def assertEqual[A: Equal](a1: A, a2: A): Assertion =
    assert(Equal[A].equal(a1, a2), s"because $a1 is not equal to $a2")

  test("addModification") {
    val mods0 = ProteinModifications(SiteLabel("a") -> SiteState("x"))

    val mods1 = mods0.addModification(SiteLabel("b"), SiteState("y"))
    assert(mods1.isAdmissible)

    val mods2 = mods1.addModification(SiteLabel("a"), SiteState("x"))
    assert(mods2.isAdmissible)

    val mods3 = mods2.addModification(SiteLabel("a"), SiteState("z"))
    assert(!mods3.isAdmissible)
  }

  test("site unification") {
    val ss1 = SiteWithState(SiteLabel("a"), SiteState("x"))
    AdmissibleProteinModifications.siteWithStateUnification.mustUnify(ss1, ss1) match {
      case None => assert(false, "unexpected unification failure")
      case Some(None) => assert(false, "unexpected non-unifiability")
      case Some(Some((d1, ss2, d2))) =>
        assertEqual(ss2, ss1)
        assert(d1 === None)
        assert(d2 === None)
    }
  }

  test("combine") {
    val mods0 = ProteinModifications(
      SiteLabel("a") -> SiteState("x"))

    val mods1 = ProteinModifications(
      SiteLabel("a") -> SiteState("x"),
      SiteLabel("b") -> SiteState("y"))

    val mods2 = ProteinModifications(
      SiteLabel("a") -> SiteState("x"),
      SiteLabel("c") -> SiteState("z"))

    val mods3 = ProteinModifications(
      SiteLabel("b") -> SiteState("Y"),
      SiteLabel("c") -> SiteState("z"))

    assertEqual(mods0 combine mods1, mods1)

    assert( (mods1 combine mods2).isAdmissible)
    assert( (mods2 combine mods3).isAdmissible)
    assert(!(mods1 combine mods3).isAdmissible)
  }

  test("refines") {
    val mods0 = ProteinModifications(SiteLabel("a") -> SiteState("x"))

    val mods1 = ProteinModifications(
      SiteLabel("a") -> SiteState("x"),
      SiteLabel("b") -> SiteState("y"))

    val mods2 = ProteinModifications(
      SiteLabel("a") -> SiteState("x"),
      SiteLabel("c") -> SiteState("z"))

    (mods1 refines mods0) match {
      case Some(refinement :: refinements) => ()
      case Some(Nil) => assert(false)
      case None => assert(false)
    }

    (mods2 refines mods0) match {
      case Some(refinement :: refinements) => ()
      case Some(Nil) => assert(false)
      case None => assert(false)
    }

    (mods2 refines mods1) match {
      case Some(Nil) => ()
      case Some(x::xs) => assert(false)
      case None => assert(false)
    }

    (mods1 refines mods2) match {
      case Some(Nil) => ()
      case Some(x::xs) => assert(false)
      case None => assert(false)
    }
  }

}
