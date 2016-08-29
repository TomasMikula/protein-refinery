package proteinrefinery.lib

import nutcracker.{Dom, Join}

import scala.collection.mutable

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

    val entries: Iterator[Option[(Site, SiteState)]] =
      (this.mods.keySet union that.mods.keySet).iterator.map {
        s => (this.get(s), that.get(s)) match {
          case (Some(s1), Some(s2)) => if (s1 == s2) Some((s, s1)) else None
          case (Some(s1), None) => Some((s, s1))
          case (None, Some(s2)) => Some((s, s2))
          case (None, None) => sys.error("Unreachable code")
        }
      }

    val builder = mutable.Map[Site, SiteState]()
    val isCollision = entries.foldLeft(false) { (collision, entry) =>
      if (collision) collision
      else entry match {
        case None => true
        case Some((s, st)) => builder += ((s, st)); false
      }
    }

    if (!isCollision) AdmissibleProteinModifications(builder.toMap)
    else InvalidProteinModifications
  }

  /** Returns ProteinModifications that is less specific than either `this` or `that`
    * and is the most specific such ProteinModifications. In other words, returns the
    * greatest lower bound of the two in the (partial) ordering given by specificity.
    */
  def meet(that: AdmissibleProteinModifications): AdmissibleProteinModifications = {
    val keys = this.mods.keySet union that.mods.keySet
    val builder = mutable.Map[Site, SiteState]()
    keys.foreach(s => (this.get(s), that.get(s)) match {
      case (Some(s1), Some(s2)) if s1 == s2 => builder.put(s, s1)
      case _ => // do nothing
    })
    AdmissibleProteinModifications(builder.toMap)
  }

  override def toString = mods.iterator.map({ case (s, st) => s"$s~$st" }).mkString("(", ",", ")")

  private def get(s: Site): Option[SiteState] = mods.get(s)
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
