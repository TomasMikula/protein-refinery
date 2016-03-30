package protein.demo

import nutcracker.PropagationLang._
import protein.{Cost, Vocabulary}
import protein.mechanism.{CompetitiveBinding, Phosphorylation}
import protein.capability.syntax._
import protein.search.PhosphorylationSearch

object Demo1_Phos extends App {
  val Solver = protein.bfsSolver

  /*
   * PROBLEM:
   * Search for a mechanism of how C can phosphorylate B.
   */
  val problem = PhosphorylationSearch.search('C, 'B)

  // output solutions
  Solver.solutions(problem).toStream.run(TestKB) foreach {
    case (ph: Phosphorylation, c: Cost) =>
      println
      println(s"Cost: $c")
      println(s"Result: $ph")
  }
  println
}


object Demo2_NegInfl extends App {
  val Solver = protein.bfsSolver

  /*
   * PROBLEM:
   * Search for a mechanism of how C can phosphorylate B
   * and a negative influence of D on this phosphorylation.
   */
  val problem = for {
    phos0 <- PhosphorylationSearch.search0('C, 'B)
    ni <- PhosphorylationSearch.negativeInfluence('D, phos0)
    phos <- PhosphorylationSearch.fetch(phos0)
    pr <- promiseC[Vocabulary].tuple(phos, ni)
  } yield pr

  // output solutions
  Solver.solutions(problem).toStream.run(TestKB) foreach {
    case ((ph: Phosphorylation, cb: CompetitiveBinding), c: Cost) =>
      println
      println(s"Cost: $c")
      println(ph)
      println(cb)
  }
  println
}
