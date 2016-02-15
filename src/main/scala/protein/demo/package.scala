package protein

import nutcracker.BFSSolver
import protein.search.PhosphorylationSearch

package object demo {
  val Solver = new BFSSolver[Cost]
  val PhosSearch = PhosphorylationSearch(TestKB)
}
