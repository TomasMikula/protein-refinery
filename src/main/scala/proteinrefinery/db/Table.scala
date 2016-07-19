package proteinrefinery.db

import nutcracker.util.Lst
import scalaz.syntax.foldable._
import scalaz.std.list._

/**
  *
  * @tparam K type of triggered thunks.
  * @tparam R type of rows held by this table.
  */
final case class Table[K, R] private(
  private val rows: List[R],
  private val triggers: List[R => Lst[K]]
) {

  def insert(row: R): (Table[K, R], Lst[K]) = {
    val ks = triggers.foldMap(_(row))
    (copy(rows = row :: rows), ks)
  }

  def query(p: R => Lst[K]): (Table[K, R], Lst[K]) = {
    val ks = rows.foldMap(p)
    (copy(triggers = p :: triggers), ks)
  }
}

object Table {
  def empty[K, R]: Table[K, R] = Table(Nil, Nil)
  def apply[K, R](rows: List[R]): Table[K, R] = Table(rows, Nil)
}