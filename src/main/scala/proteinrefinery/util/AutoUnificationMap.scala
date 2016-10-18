package proteinrefinery.util

class AutoUnificationMap[K, V] private[util](val entries: List[(K, V)]) extends AnyVal {

  def size: Int = entries.size

  def mapValues[W](f: V => W): AutoUnificationMap[K, W] =
    new AutoUnificationMap(entries.map(kv => (kv._1, f(kv._2))))

  /** Returns a list of values whose keys must be unified with the given key. */
  def get(k: K)(implicit I: Identification[K]): List[V] =
    entries.foldLeft[List[V]](Nil)((vs, kv) => {
      val (k0, v) = kv
      I.unifyIfNecessary(k0, k) match {
        case Some(_) => v :: vs
        case None => vs
      }
    })

  /** Returns a list of entries whose keys must be unified with the given key. */
  def getEntries(k: K)(implicit I: Identification[K]): List[(K, V)] =
    entries.foldLeft[List[(K, V)]](Nil)((kvs, kv) => {
      val (k0, v) = kv
      I.unifyIfNecessary(k0, k) match {
        case Some(_) => (k0, v) :: kvs
        case None => kvs
      }
    })

  /** Returns the aggregate of the values whose keys must be unified with the given key,
    * as aggregated by the given function.
    */
  def getUnifiedValue(k: K)(f: (V, V) => V)(implicit I: Identification[K]): Option[V] =
    get(k) match {
      case v::vs => Some(vs.foldLeft(v)(f))
      case Nil => None
    }

  /** Like `getUnifiedValue`, but also returns the unification of the keys. */
  def getUnifiedEntry(k: K)(f: (V, V) => V)(implicit I: Identification[K]): Option[(K, V)] =
    entries.foldLeft[(K, Option[V])]((k, None))((acc, kv) => {
      val (k0, v0) = kv
      val (accK, accV) = acc
      I.unifyIfNecessary(k0, accK) match {
        case Some((_, k1, _)) => accV match {
          case Some(v) => (k1, Some(f(v, v0)))
          case None => (k1, Some(v0))
        }
        case None => acc
      }
    }) match {
      case (k, Some(v)) => Some((k, v))
      case (_, None)    => None
    }

  /** Add an entry to the map. The new key might be unified with some old keys, in which case
    * the values are combined using the given (commutative) function.
    */
  def put(k: K, v: V)(f: (V, V) => V)(implicit I: Identification[K]): AutoUnificationMap[K, V] = {
    val (k1, v1, untouched) = collect(k, v)(f)
    new AutoUnificationMap((k1, v1) :: untouched)
  }

  /** Equivalent to `put`ting every entry from `that` map into this map. */
  def union(that: AutoUnificationMap[K, V])(f: (V, V) => V)(implicit I: Identification[K]): AutoUnificationMap[K, V] =
    that.entries.foldLeft[AutoUnificationMap[K, V]](this)((acc, kv) => acc.put(kv._1, kv._2)(f))

  /** From both maps, retain only those entries whose key must be unified with some key from the other map.
    * The result is a union of those entries, with values combined using the given (commutative) function.
    */
  def intersect(that: AutoUnificationMap[K, V])(f: (V, V) => V)(implicit I: Identification[K]): AutoUnificationMap[K, V] =
    that.entries.foldLeft[(AutoUnificationMap[K, V], AutoUnificationMap[K, V])]((AutoUnificationMap.empty, this))((acc_thisRemainder, thatEntry) => {
      val (acc, thisRemainder) = acc_thisRemainder
      val (k, v) = thatEntry
      thisRemainder.collect(k, v)(f) match {
        case (k, v, thisUntouched) =>
          if(thisUntouched.size < thisRemainder.size) // (k, v) unified with something from the remainder of this map => add to the accumulator
            (acc.put(k, v)(f), new AutoUnificationMap(thisUntouched))
          else // k did not unify with anything from the remainder of this map, try accumulator
            acc.collect(k, v)(f) match {
              case (k, v, accUntouched) =>
                if(accUntouched.size < acc.size) // unified with something in the accumulator => add to the accumulator
                  (new AutoUnificationMap((k, v)::accUntouched), thisRemainder)
                else // did not unify with anything, ignore the entry
                  (acc, thisRemainder)
            }
      }
    })._1 // ignore the remainder of this map that didn't unify with anything. only return the accumulator

  /** Accumulates every entry whose key must be unified with the given key. Also returns a list of untouched elements.
    * When nothing is unified, the size of returned list of untouched entries is the same as the size of this map,
    * and the returned key-value pair is `(k, v)`.
    */
  private def collect(k: K, v: V)(f: (V, V) => V)(implicit I: Identification[K]): (K, V, List[(K, V)]) =
    entries.foldLeft[(K, V, List[(K, V)])]((k, v, Nil))((acc_untouched, kv) => {
      val (accK, accV, untouched) = acc_untouched
      val (k, v) = kv
      I.unifyIfNecessary(accK, k) match {
        case Some((_, k1, _)) => (k1, f(accV, v), untouched)
        case None => (accK, accV, kv :: untouched)
      }
    })
}

object AutoUnificationMap {
  def empty[K, V]: AutoUnificationMap[K, V] = new AutoUnificationMap(Nil)
}