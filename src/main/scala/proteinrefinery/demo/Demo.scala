package proteinrefinery.demo

import nutcracker._
import proteinrefinery.lib.{NegativeInfluenceOnPhosphorylation, Phosphorylation}
import proteinrefinery.capability.syntax._

object Demo1_Phos extends App {

  /*
   * PROBLEM:
   * Search for a mechanism of how C can phosphorylate B.
   */
  val problem = Phosphorylation.search('C, 'B)

  // output solutions
  val (s, ref) = proteinrefinery.interpreterF(problem)(proteinrefinery.initialState(TestKB))
  proteinrefinery.fetch(ref)(s).value foreach { ph =>
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
    phos <- Phosphorylation.searchC('C, 'B)
    ni <- NegativeInfluenceOnPhosphorylation.searchC('D, phos)
  } yield (phos, ni))

  // output solutions
  val (s, ref) = proteinrefinery.interpreterF(problem)(proteinrefinery.initialState(TestKB))
  proteinrefinery.fetch(ref)(s).value foreach {
    case (ph: Phosphorylation, ni: NegativeInfluenceOnPhosphorylation) =>
      println
      println(ph)
      println(ni)
  }
  println
}
