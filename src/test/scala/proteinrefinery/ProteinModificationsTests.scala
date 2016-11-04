package proteinrefinery

import nutcracker.DRef
import org.scalatest.{Assertion, FunSuite}
import proteinrefinery.lib.Site._
import proteinrefinery.util.Identification.Syntax._

import scalaz.Equal

class ProteinModificationsTests extends FunSuite {
  val lib = new Lib[Prg, DRef]
  import lib._

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

  test("site identification") {
    val s = Site.fromLabel(SiteLabel("a"))
    (s unifyIfNecessary s) match {
      case None => assert(false, "unexpected non-obligation to unify")
      case Some((d1, s2, d2)) =>
        assertEqual(s2, s)
        assert(d1 === None)
        assert(d2 === None)
    }
  }

  test("site-with-state identification") {
    val ss1 = SiteWithState(SiteLabel("a"), SiteState("x"))
    (ss1 unifyIfNecessary ss1) match {
      case None => assert(false, "unexpected non-obligation to unify")
      case Some((d1, ss2, d2)) =>
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
      case refinement :: refinements => ()
      case Nil => assert(false)
    }

    (mods2 refines mods0) match {
      case refinement :: refinements => ()
      case Nil => assert(false)
    }

    (mods2 refines mods1) match {
      case Nil => ()
      case x::xs => assert(false)
    }

    (mods1 refines mods2) match {
      case Nil => ()
      case x::xs => assert(false)
    }
  }

}
