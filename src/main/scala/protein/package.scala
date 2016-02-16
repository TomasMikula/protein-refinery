import nutcracker.util.free.FreeK

import scala.language.higherKinds

import nutcracker.PropBranchRelCost

import scalaz.{Apply, Applicative, Monad}

package object protein {

  val SearchLang = new PropBranchRelCost[Cost]

  type Vocabulary[K[_], A] = SearchLang.Vocabulary[K, A]

  type Cont[A] = scalaz.Cont[FreeK[Vocabulary, Unit], A]
}
