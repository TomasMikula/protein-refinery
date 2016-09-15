package proteinrefinery.util

import scala.collection.mutable.{Map => MMap}

package object syntax {

  implicit class IteratorOps[A](it: Iterator[A]) {
    def toMultiMap[K, V](implicit ev: A =:= (K, V)): Map[K, List[V]] = {
      val builder = MMap[K, List[V]]()
      while(it.hasNext) {
        val (k, v) = ev(it.next())
        builder.update(k, v :: builder.getOrElse(k, Nil))
      }
      builder.toMap
    }

    def collectToList[B](f: A => Option[B]): List[B] = {
      var buf = List.newBuilder[B]
      while(it.hasNext) {
        f(it.next()).foreach(buf += _)
      }
      buf.result()
    }

    def mapFilter[B](f: A => Option[B]): Iterator[B] = {
      it.flatMap(a => f(a) match {
        case Some(b) => Iterator.single(b)
        case None => Iterator.empty
      })
    }
  }

  implicit class IterableOps[A](col: Iterable[A]) {
    def toMultiMap[K, V](implicit ev: A =:= (K, V)): Map[K, List[V]] =
      col.iterator.toMultiMap

    def collectToList[B](f: A => Option[B]): List[B] =
      col.iterator.collectToList(f)
  }

  implicit class MapOps[K, V](self: Map[K, V]) {
    def pairWith[W](that: Map[K, W])(v0: V, w0: W): Map[K, (V, W)] = {
      val keys = self.keySet union that.keySet
      var builder = Map.newBuilder[K, (V, W)]
      keys.foreach(a => {
        val b = self.get(a).getOrElse(v0)
        val c = that.get(a).getOrElse(w0)
        builder += ((a, (b, c)))
      })
      builder.result()
    }

    def pairWithOpt[W](that: Map[K, W]): Map[K, (Option[V], Option[W])] = {
      val keys = self.keySet union that.keySet
      var builder = Map.newBuilder[K, (Option[V], Option[W])]
      keys.foreach(a => builder += ((a, (self.get(a), that.get(a)))))
      builder.result()
    }

    def splitKeys[K1, K2](f: K => Either[K1, K2]): (Map[K1, V], Map[K2, V]) =
      mapSplitKeys(self)(f)

    def splitValues[V1, V2](f: V => Either[V1, V2]): (Map[K, V1], Map[K, V2]) =
      mapSplitValues(self)(f)
  }
}
