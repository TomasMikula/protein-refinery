package proteinrefinery.db

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK, Lst}

import scalaz.~>

sealed trait DBLang[K[_], A]

object DBLang {
  final case class Insert[K[_], R](table: TableId[R], r: R) extends DBLang[K, Unit]
  final case class Query[K[_], R](table: TableId[R], p: R => Lst[K[Unit]]) extends DBLang[K, Unit]

  def insert[K[_], R](table: TableId[R], r: R): DBLang[K, Unit] = Insert(table, r)
  def query[K[_], R](table: TableId[R])(p: R => Lst[K[Unit]]): DBLang[K, Unit] = Query(table, p)

  def insertF[F[_[_], _], R](table: TableId[R], r: R)(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[DBLang, F, Unit](insert(table, r))
  def queryF[F[_[_], _], R](table: TableId[R])(p: R => Lst[FreeK[F, Unit]])(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF[DBLang, F, Unit](query(table)(p))

  implicit def functorKAInstance: FunctorKA[DBLang] = new FunctorKA[DBLang] {
    def transform[K[_], L[_], A](fk: DBLang[K, A])(f: K ~> L): DBLang[L, A] = fk match {
      case Insert(t, r) => Insert(t, r)
      case Query(t, p) => query(t)(r => p(r).map(f(_)))
    }
  }
}