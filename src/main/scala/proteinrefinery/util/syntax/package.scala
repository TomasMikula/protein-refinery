package proteinrefinery.util

import scala.annotation.tailrec
import scalaz.\&/

package object syntax {

  implicit class ListOps[A](l: List[A]) {
    def split[B, C](f: A => Either[B, C]): (List[B], List[C]) = {
      val builderB = List.newBuilder[B]
      val builderC = List.newBuilder[C]
      l.foreach(f(_) match {
        case Left(b) => builderB += b
        case Right(c) => builderC += c
      })
      (builderB.result(), builderC.result())
    }

    def removeFirst(p: A => Boolean): Option[(A, List[A])] = {
      val i = l.indexWhere(p)
      if(i < 0) None
      else {
        @tailrec
        def go(n: Int, rev: List[A], tail: List[A]): (A, List[A]) =
          if(n == 0) (tail.head, rev reverse_::: tail.tail)
          else go(n-1, tail.head :: rev, tail.tail)

        Some(go(i, Nil, l))
      }
    }
  }

  implicit class MapOps[K, V](self: Map[K, V]) {
    def pairWith[W](that: Map[K, W])(v0: V, w0: W): Map[K, (V, W)] = {
      val keys = self.keySet union that.keySet
      val builder = Map.newBuilder[K, (V, W)]
      keys.foreach(a => {
        val b = self.get(a).getOrElse(v0)
        val c = that.get(a).getOrElse(w0)
        builder += ((a, (b, c)))
      })
      builder.result()
    }

    def pairWithOpt[W](that: Map[K, W]): Map[K, V \&/ W] = {
      val keys = self.keySet union that.keySet
      val builder = Map.newBuilder[K, V \&/ W]
      keys.foreach(a => builder += ((a, (self.get(a), that.get(a)) match {
        case (Some(v), Some(w)) => \&/.Both(v, w)
        case (Some(v), None) => \&/.This(v)
        case (None, Some(w)) => \&/.That(w)
        case _ => sys.error("Unreachable code")
      })))
      builder.result()
    }

    def splitKeys[K1, K2](f: K => Either[K1, K2]): (Map[K1, V], Map[K2, V]) =
      mapSplitKeys(self)(f)

    def splitValues[V1, V2](f: V => Either[V1, V2]): (Map[K, V1], Map[K, V2]) =
      mapSplitValues(self)(f)
  }
}
