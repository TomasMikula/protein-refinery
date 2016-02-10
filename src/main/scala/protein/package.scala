import nutcracker.util.free.FreeK

import scala.language.higherKinds

import nutcracker.PropBranchPromCost

import scalaz.{Apply, Applicative, Monad}

package object protein {

  val SearchLang = new PropBranchPromCost[Cost]

  type Vocabulary[K[_], A] = SearchLang.Vocabulary[K, A]

  type Cont[A] = scalaz.Cont[FreeK[Vocabulary, Unit], A]
}
