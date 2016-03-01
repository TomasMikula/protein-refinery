package protein

import nutcracker.PropCost
import protein.search.PhosphorylationSearch

package object demo {
  val Solver = new PropCost[Cost].bfsSolver
  val PhosSearch = PhosphorylationSearch(TestKB)
}
