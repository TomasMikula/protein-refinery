import nutcracker.PropCost
import nutcracker.util.free.FreeK
import scala.language.higherKinds

package object protein {

  val SearchLang = new PropCost[Cost]

  type Vocabulary[K[_], A] = SearchLang.Vocabulary[K, A]

  type Cont[A] = scalaz.Cont[FreeK[Vocabulary, Unit], A]
}
