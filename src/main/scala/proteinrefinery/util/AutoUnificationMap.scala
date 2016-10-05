package proteinrefinery.util

import scala.language.higherKinds
import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.foldable._

class AutoUnificationMap[K, V] private[util](val entries: List[(K, V)]) extends AnyVal {

  def size: Int = entries.size

  def mapValues[W](f: V => W): AutoUnificationMap[K, W] =
    new AutoUnificationMap(entries.map(kv => (kv._1, f(kv._2))))

  /** Returns a list of values whose keys must be unified with the given key. */
  def get[M[_]](k: K)(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[List[V]] =
    entries.foldLeftM[M, List[V]](Nil)((vs, kv) => {
      val (k0, v) = kv
      U.mustUnify(k0, k).map({
        case Some(_) => v :: vs
        case None => vs
      })
    })

  /** Returns a list of entries whose keys must be unified with the given key. */
  def getEntries[M[_]](k: K)(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[List[(K, V)]] =
    entries.foldLeftM[M, List[(K, V)]](Nil)((kvs, kv) => {
      val (k0, v) = kv
      U.mustUnify(k0, k).map({
        case Some(_) => (k0, v) :: kvs
        case None => kvs
      })
    })

  /** Returns the aggregate of the values whose keys must be unified with the given key,
    * as aggregated by the given function.
    */
  def getUnifiedValue[M[_]](k: K)(f: (V, V) => M[V])(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[Option[V]] =
    M.bind(get[M](k))({
      case v::vs => vs.foldLeftM(v)(f).map(Some(_))
      case Nil => M.point(None)
    })

  /** Like `getUnifiedValue`, but also returns the unification of the keys. */
  def getUnifiedEntry[M[_]](k: K)(f: (V, V) => M[V])(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[Option[(K, V)]] =
    entries.foldLeftM[M, (K, Option[V])]((k, None))((acc, kv) => {
      val (k0, v0) = kv
      val (accK, accV) = acc
      U.mustUnify(k0, accK).flatMap[(K, Option[V])]({
        case Some((_, k1, _)) => accV match {
          case Some(v) => f(v, v0).map(v1 => (k1, Some(v1)))
          case None => M.point((k1, Some(v0)))
        }
        case None => M.point(acc)
      })
    }).map({
      case (k, Some(v)) => Some((k, v))
      case (_, None)    => None
    })

  /** Add an entry to the map. The new key might be unified with some old keys, in which case
    * the values are combined using the given (commutative) function.
    */
  def put[M[_]](k: K, v: V)(f: (V, V) => M[V])(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[AutoUnificationMap[K, V]] =
    collect(k, v)(f).map({ case (k, v, untouched) => new AutoUnificationMap((k, v) :: untouched) })

  /** Equivalent to `put`ting every entry from `that` map into this map. */
  def union[M[_]](that: AutoUnificationMap[K, V])(f: (V, V) => M[V])(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[AutoUnificationMap[K, V]] =
    that.entries.foldLeftM[M, AutoUnificationMap[K, V]](this)((acc, kv) => acc.put(kv._1, kv._2)(f))

  /** From both maps, retain only those entries whose key must be unified with some key from the other map.
    * The result is a union of those entries, with values combined using the given (commutative) function.
    */
  def intersect[M[_]](that: AutoUnificationMap[K, V])(f: (V, V) => M[V])(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[AutoUnificationMap[K, V]] =
    that.entries.foldLeftM[M, (AutoUnificationMap[K, V], AutoUnificationMap[K, V])]((AutoUnificationMap.empty, this))((acc_thisRemainder, thatEntry) => {
      val (acc, thisRemainder) = acc_thisRemainder
      val (k, v) = thatEntry
      thisRemainder.collect(k, v)(f).flatMap({
        case (k, v, thisUntouched) =>
          if(thisUntouched.size < thisRemainder.size) // (k, v) unified with something from the remainder of this map => add to the accumulator
            acc.put(k, v)(f).map((_, new AutoUnificationMap(thisUntouched)))
          else // k did not unify with anything from the remainder of this map, try accumulator
            acc.collect(k, v)(f).map({
              case (k, v, accUntouched) =>
                if(accUntouched.size < acc.size) // unified with something in the accumulator => add to the accumulator
                  (new AutoUnificationMap((k, v)::accUntouched), thisRemainder)
                else // did not unify with anything, ignore the entry
                  (acc, thisRemainder)
            })
      })
    }).map(_._1) // ignore the remainder of this map that didn't unify with anything. only return the accumulator

  /** Accumulates every entry whose key must be unified with the given key. Also returns a list of untouched elements.
    * When nothing is unified, the size of returned list of untouched entries is the same as the size of this map,
    * and the returned key-value pair is `(k, v)`.
    */
  private def collect[M[_]](k: K, v: V)(f: (V, V) => M[V])(implicit U: Unification.Aux0[K, M], M: Monad[M]): M[(K, V, List[(K, V)])] =
    entries.foldLeftM[M, (K, V, List[(K, V)])]((k, v, Nil))((acc_untouched, kv) => {
      val (accK, accV, untouched) = acc_untouched
      val (k, v) = kv
      U.mustUnify(accK, k).flatMap({
        case Some((_, k1, _)) => f(accV, v).map((k1, _, untouched))
        case None => M.point((accK, accV, kv :: untouched))
      })
    })
}

object AutoUnificationMap {
  def empty[K, V]: AutoUnificationMap[K, V] = new AutoUnificationMap(Nil)
}