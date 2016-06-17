package protein.db

import scala.language.higherKinds
import nutcracker.util.{KMap, Lst}

import scalaz.{Lens, State}

/**
  *
  * @tparam K type representing triggered thunks.
  */
final case class DB[K] private(private val tables: KMap[TableId, Table[K, ?]]) {

  def insert[R](table: TableId[R], r: R): (DB[K], Lst[K]) =
    DB.insert[K, R](table, r).run(this)

  def query[R](table: TableId[R])(p: R => Lst[K]): (DB[K], Lst[K]) =
    DB.query(table, p).run(this)

  def setTable[R](id: TableId[R], t: Table[K, R]): DB[K] =
    DB(tables.updated(id, t))
}

object DB {

  def empty[K]: DB[K] = DB(KMap[TableId, Table[K, ?]]())

  private def tables[K]: Lens[DB[K], KMap[TableId, Table[K, ?]]] =
    Lens.lensu[DB[K], KMap[TableId, Table[K, ?]]](
      (db, ts) => db.copy(tables = ts), // set
      _.tables // get
    )

  private def getOrInsert[K[_], V[_], A](k: K[A], v: => V[A]): State[KMap[K, V], V[A]] =
    State(m => m.get(k) match {
      case Some(v) => (m, v)
      case None =>
        val w = v
        (m.updated(k, w), w)
    })

  private def getTable[K, R](id: TableId[R]): State[DB[K], Table[K, R]] =
    getOrInsert[TableId, Table[K, ?], R](id, Table.empty[K, R]).zoom(tables[K])

  private def setTable[K, R](id: TableId[R], t: Table[K, R]): State[DB[K], Unit] =
    State(db => (db.copy(tables = db.tables.updated(id, t)), ()))

  private def insert[K, R](table: TableId[R], r: R): State[DB[K], Lst[K]] =
    for {
      t <- getTable[K, R](table)
      (t1, ks) = t.insert(r)
      _ <- setTable(table, t1)
    } yield ks

  private def query[K, R](table: TableId[R], p: R => Lst[K]): State[DB[K], Lst[K]] =
    for {
      t <- getTable[K, R](table)
      (t1, ks) = t.query(p)
      _ <- setTable(table, t1)
    } yield ks
}