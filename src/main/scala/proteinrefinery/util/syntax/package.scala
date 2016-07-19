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

  implicit class MapOps[A, B](self: Map[A, B]) {
    def pairWith[C](that: Map[A, C])(defaultB: B, defaultC: C): Map[A, (B, C)] = {
      val keys = self.keySet union that.keySet
      var builder = Map.newBuilder[A, (B, C)]
      keys.foreach(a => {
        val b = self.get(a).getOrElse(defaultB)
        val c = that.get(a).getOrElse(defaultC)
        builder += ((a, (b, c)))
      })
      builder.result()
    }

    def pairWithOpt[C](that: Map[A, C]): Map[A, (Option[B], Option[C])] = {
      val keys = self.keySet union that.keySet
      var builder = Map.newBuilder[A, (Option[B], Option[C])]
      keys.foreach(a => builder += ((a, (self.get(a), that.get(a)))))
      builder.result()
    }
  }
}
