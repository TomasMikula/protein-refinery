package protein.demo

import nutcracker.BFSSolver
import nutcracker.PromiseLang._
import protein.{Vocabulary, Cost}
import protein.mechanism.{CompetitiveBinding, Phosphorylation, Assoc}
import protein.search.{PhosphorylationSearch, AssocSearch}
import protein.capability.syntax._

object Demo extends App {

  val solver = new BFSSolver[Cost]
  val PhosSearch = PhosphorylationSearch(TestKB)


  /*
   * Example 1:
   * Search for any association between proteins C and B
   */

  // problem statement
  val problem1 = AssocSearch(TestKB).search('C, 'B)

  // output solutions
  solver.solutions(problem1).toStream foreach {
    case (a: Assoc, c: Cost) =>
      println
      println(s"Cost: $c")
      println(s"Result: $a")
  }


  println; println


  /*
   * Example 2:
   * Search for a mechanism of how C can phosphorylate B
   * and a negative influence of D on this phosphorylation.
   */

  // problem statement
  val problem2 = for {
    phos0 <- PhosSearch.search0('C, 'B)
    ni <- PhosSearch.negativeInfluence('D, phos0)
    phos <- PhosSearch.fetch(phos0)
    pr <- promiseC[Vocabulary].tuple(phos, ni)
  } yield pr

  // output solutions
  solver.solutions(problem2).toStream foreach {
    case ((ph: Phosphorylation, cb: CompetitiveBinding), c: Cost) =>
      println
      println(s"Cost: $c")
      println(ph)
      println(cb)
  }

}
