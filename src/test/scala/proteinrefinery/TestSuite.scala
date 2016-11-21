package proteinrefinery

import scala.language.higherKinds
import nutcracker.util.{DeepEqual, HEqualK}
import org.scalactic.{Prettifier, source}
import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import org.scalatest.{Assertion, FunSuite, Succeeded}

import scalaz.{Equal, NaturalTransformation, ~>}
import scalaz.Id._

class TestSuite extends FunSuite {

  def assertEqual[A](expected: A)(actual: A)(implicit ev: Equal[A], prettifier: Prettifier, pos: source.Position): Assertion = {
    if (!ev.equal(actual, expected)) {
      val s = s"Expected: $expected, but got $actual"
      throw new TestFailedException((_: StackDepthException) => Some(s), None, pos)
    }
    Succeeded
  }

  def assertDeepEqual[A1, A2, Ptr1[_], Ptr2[_]](expected: Ptr1[A1])(actual: Ptr2[A2])(implicit
    ev: DeepEqual[A1, A2, Ptr1, Ptr2],
    eq2: HEqualK[Ptr2],
    deref1: Ptr1 ~> Id,
    deref2: Ptr2 ~> Id,
    prettifier: Prettifier,
    pos: source.Position
  ): Assertion = {
    if (!ev.lift.deepEqual(expected, actual)(deref1, deref2)) {
      val s = s"Expected: $expected, but got $actual"
      throw new TestFailedException((_: StackDepthException) => Some(s), None, pos)
    }
    Succeeded
  }

  implicit val refl: Id ~> Id = NaturalTransformation.refl
}
