package protein.demo

import nutcracker._
import protein.{Cost, Vocabulary}
import protein.mechanism.{CompetitiveBinding, Phosphorylation}
import protein.capability.syntax._
import protein.search.PhosSearch

object Demo1_Phos extends App {

  /*
   * PROBLEM:
   * Search for a mechanism of how C can phosphorylate B.
   */
  val problem = PhosSearch.search('C, 'B)

  // output solutions
  val (s, ref) = protein.interpreter(problem)(protein.initialState(TestKB))
  protein.fetch(ref)(s).value foreach { ph =>
    println
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
  val problem = IncSet.collect(for {
    phos <- PhosSearch.searchC('C, 'B)
    ni <- PhosSearch.negativeInfluenceC('D, phos)
  } yield (phos, ni))

  // output solutions
  val (s, ref) = protein.interpreter(problem)(protein.initialState(TestKB))
  protein.fetch(ref)(s).value foreach {
    case (ph: Phosphorylation, cb: CompetitiveBinding) =>
      println
      println(ph)
      println(cb)
  }
  println
}
