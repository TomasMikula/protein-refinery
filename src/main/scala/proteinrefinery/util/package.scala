package proteinrefinery

import scala.collection.mutable
import scala.language.higherKinds
import scalaz.{Applicative, BindRec, \/}
import scalaz.std.list._
import scalaz.syntax.functor._
import scalaz.syntax.traverse0._

package object util {

  def mapUnion[K, V, M[_]](m1: Map[K, V], m2: Map[K, V])(f: (V, V) => M[V])(implicit M: Applicative[M]): M[Map[K, V]] = {
    (m1.keySet union m2.keySet).iterator.map(k => (m1.get(k), m2.get(k)) match {
      case (Some(v1), Some(v2)) => M.map(f(v1, v2))((k, _))
      case (Some(v1), None) => M.point((k, v1))
      case (None, Some(v2)) => M.point((k, v2))
      case (None, None) => sys.error("Unreachable code")
    }).toList.sequence.map(_.toMap)
  }

  def buildMap[K, V, M[_]](kvs: Iterator[(K, V)])(f: (V, V) => M[V])(implicit M0: Applicative[M], M1: BindRec[M]): M[Map[K, V]] =
    M1.tailrecM(mutable.Map[K, V]())(builder => {
      if(kvs.hasNext) {
        val (k, v) = kvs.next()
        builder.get(k) match {
          case Some(v0) => M0.map(f(v0, v))(v => { builder.put(k, v); \/.left(builder)})
          case None => builder.put(k, v); M0.point(\/.left(builder))
        }
      } else {
        M0.point(\/.right(builder.toMap))
      }
    })

  def mapSplitValues[K, V, A, B](m: Map[K, V])(f: V => Either[A, B]): (Map[K, A], Map[K, B]) = {
    val builderA = mutable.Map[K, A]()
    val builderB = mutable.Map[K, B]()

    m.foreach(kv => {
      val (k, v) = kv
      f(v).fold(builderA.put(k, _), builderB.put(k, _))
    })

    (builderA.toMap, builderB.toMap)
  }

  def mapSplitKeys[K, V, A, B](m: Map[K, V])(f: K => Either[A, B]): (Map[A, V], Map[B, V]) = {
    val builderA = mutable.Map[A, V]()
    val builderB = mutable.Map[B, V]()

    m.foreach(kv => {
      val (k, v) = kv
      f(k).fold(builderA.put(_, v), builderB.put(_, v))
    })

    (builderA.toMap, builderB.toMap)
  }

  def mapIntersect[K, V1, V2, V](m1: Map[K, V1], m2: Map[K, V2])(f: (V1, V2) => Option[V]): Map[K, V] = {
    val keys = m1.keySet intersect m2.keySet
    val builder = mutable.Map[K, V]()
    keys.foreach(k => f(m1(k), m2(k)).foreach(builder.put(k, _)))
    builder.toMap
  }

}
