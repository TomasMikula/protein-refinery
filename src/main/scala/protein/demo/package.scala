package protein

import protein.search.PhosphorylationSearch

package object demo {
  val Solver = protein.bfsSolver
  val PhosSearch = PhosphorylationSearch(TestKB)
}
