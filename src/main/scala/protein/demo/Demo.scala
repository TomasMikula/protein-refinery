package protein.demo

import nutcracker.PropagationLang._
import protein.{Vocabulary, Cost}
import protein.mechanism.{CompetitiveBinding, Phosphorylation}
import protein.capability.syntax._

object Demo1_Phos extends App {

  /*
   * PROBLEM:
   * Search for a mechanism of how C can phosphorylate B.
   */
  val problem = PhosSearch.search('C, 'B)

  // output solutions
  Solver.solutions(problem).toStream foreach {
    case (ph: Phosphorylation, c: Cost) =>
      println
      println(s"Cost: $c")
      println(s"Result: $ph")
  }
  println
}


object Demo2_NegInfl extends App {

  /*
   * PROBLEM:
   * Search for a mechanism of how C can phosphorylate B
   * and a negative influence of D on this phosphorylation.
   */
  val problem = for {
    phos0 <- PhosSearch.search0('C, 'B)
    ni <- PhosSearch.negativeInfluence('D, phos0)
    phos <- PhosSearch.fetch(phos0)
    pr <- promiseC[Vocabulary].tuple(phos, ni)
  } yield pr

  // output solutions
  Solver.solutions(problem).toStream foreach {
    case ((ph: Phosphorylation, cb: CompetitiveBinding), c: Cost) =>
      println
      println(s"Cost: $c")
      println(ph)
      println(cb)
  }
  println
}
