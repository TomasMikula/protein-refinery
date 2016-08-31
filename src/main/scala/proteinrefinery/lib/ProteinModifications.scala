package proteinrefinery.lib

import nutcracker.{Dom, Join}

import scala.collection.mutable
import scala.language.higherKinds
import scalaz.Applicative
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.list._
import scalaz.syntax.traverse._

sealed trait ProteinModifications {

  def isAdmissible: Boolean = this match {
    case AdmissibleProteinModifications(_) => true
    case InvalidProteinModifications => false
  }

  /** `ProteinModifications` is isomorphic to `Option[AdmissibleProteinModifications]` */
  def toOption: Option[AdmissibleProteinModifications] = this match {
    case a @ AdmissibleProteinModifications(_) => Some(a)
    case InvalidProteinModifications => None
  }

  def addModification(site: Site, state: SiteState): ProteinModifications = this match {
    case AdmissibleProteinModifications(mods) =>
      mods.get(site).fold[ProteinModifications](
        AdmissibleProteinModifications(mods.updated(site, state)))(
        st => if(st == state) this else InvalidProteinModifications
      )
    case InvalidProteinModifications => InvalidProteinModifications
  }

  def combine(that: ProteinModifications): ProteinModifications = (this, that) match {
    case (a @ AdmissibleProteinModifications(_), b @ AdmissibleProteinModifications(_)) => a.combine0(b)
    case _ => InvalidProteinModifications
  }
}

case object InvalidProteinModifications extends ProteinModifications

case class AdmissibleProteinModifications(mods: Map[Site, SiteState]) extends ProteinModifications {

  def combine0(that: AdmissibleProteinModifications): ProteinModifications = {
    mapUnion(this.mods, that.mods)((st1, st2) =>
      if(st1 == st2) Option(st1)
      else None
    ).fold[ProteinModifications](InvalidProteinModifications)(AdmissibleProteinModifications(_))
  }

  /** Returns ProteinModifications that is less specific than either `this` or `that`
    * and is the most specific such ProteinModifications. In other words, returns the
    * greatest lower bound of the two in the (partial) ordering given by specificity.
    */
  def meet(that: AdmissibleProteinModifications): AdmissibleProteinModifications = {
    AdmissibleProteinModifications(mapIntersect(this.mods, that.mods)((st1, st2) => if(st1 == st2) Some(st1) else None))
  }

  override def toString = mods.iterator.map({ case (s, st) => s"$s~$st" }).mkString("(", ",", ")")

  private def mapUnion[K, V, M[_]](m1: Map[K, V], m2: Map[K, V])(f: (V, V) => M[V])(implicit M: Applicative[M]): M[Map[K, V]] = {
    (m1.keySet union m2.keySet).iterator.map(k => (m1.get(k), m2.get(k)) match {
      case (Some(v1), Some(v2)) => M.map(f(v1, v2))((k, _))
      case (Some(v1), None) => M.point((k, v1))
      case (None, Some(v2)) => M.point((k, v2))
      case (None, None) => sys.error("Unreachable code")
    }).toList.sequence.map(_.toMap)
  }

  private def mapIntersect[K, V](m1: Map[K, V], m2: Map[K, V])(f: (V, V) => Option[V]): Map[K, V] = {
    val keys = m1.keySet intersect m2.keySet
    val builder = mutable.Map[K, V]()
    keys.foreach(k => f(m1(k), m2(k)).foreach(builder.put(k, _)))
    builder.toMap
  }
}

object ProteinModifications {
  def noModifications: AdmissibleProteinModifications = AdmissibleProteinModifications(Map())

  implicit def domInstance: Dom.Aux[ProteinModifications, Join[ProteinModifications], Unit] =
    new Dom[ProteinModifications] {
      type Update = Join[ProteinModifications]
      type Delta = Unit
      override def assess(d: ProteinModifications): Dom.Status[Join[ProteinModifications]] = d match {
        case InvalidProteinModifications => Dom.Failed
        case AdmissibleProteinModifications(_) => Dom.Refined
      }

      override def update(d: ProteinModifications, u: Join[ProteinModifications]): Option[(ProteinModifications, Unit)] = {
        val res = (d, u.value) match {
          case (p @ AdmissibleProteinModifications(_), q @ AdmissibleProteinModifications(_)) => p combine q
          case _ => InvalidProteinModifications
        }
        if(res == d) None else Some((res, ()))
      }

      override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
    }
}
