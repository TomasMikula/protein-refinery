package proteinrefinery.lib

import nutcracker.{Dom, Join}

import scala.collection.mutable

case class ProteinModifications(mods: Map[Site, SiteState]) extends AnyVal {

  def get(s: Site): Option[SiteState] = mods.get(s)

  def addModification(site: Site, state: SiteState): Option[ProteinModifications] = {
    mods.get(site).fold(
      Option(ProteinModifications(mods.updated(site, state))))(
      st => if(st == state) Some(this) else None
    )
  }

  def combine(that: ProteinModifications): Option[ProteinModifications] = {

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

    if (!isCollision) Some(ProteinModifications(builder.toMap))
    else None
  }

  /** Returns ProteinModifications that is less specific than either `this` or `that`
    * and is the most specific such ProteinModifications. In other words, returns the
    * greatest lower bound of the two in the (partial) ordering given by specificity.
    */
  def meet(that: ProteinModifications): ProteinModifications = {
    val keys = this.mods.keySet union that.mods.keySet
    val builder = mutable.Map[Site, SiteState]()
    keys.foreach(s => (this.get(s), that.get(s)) match {
      case (Some(s1), Some(s2)) if s1 == s2 => builder.put(s, s1)
      case _ => // do nothing
    })
    ProteinModifications(builder.toMap)
  }

  override def toString = mods.iterator.map({ case (s, st) => s"$s~$st" }).mkString("(", ",", ")")
}

object ProteinModifications {
  def noModifications: ProteinModifications = ProteinModifications(Map())

  type ProteinModificationsLattice = Option[ProteinModifications]

  implicit def proteinModificationsLattice: Dom.Aux[ProteinModificationsLattice, Join[ProteinModificationsLattice], Unit] =
    new Dom[ProteinModificationsLattice] {
      type Update = Join[ProteinModificationsLattice]
      type Delta = Unit
      override def assess(d: ProteinModificationsLattice): Dom.Status[Join[ProteinModificationsLattice]] = d match {
        case None => Dom.Failed
        case Some(a) => Dom.Refined
      }

      override def update(d: ProteinModificationsLattice, u: Join[ProteinModificationsLattice]): Option[(ProteinModificationsLattice, Unit)] = {
        val res = (d, u.value) match {
          case (Some(p), Some(q)) => p combine q
          case _ => None
        }
        if(res == d) None else Some((res, ()))
      }

      override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
    }
}
