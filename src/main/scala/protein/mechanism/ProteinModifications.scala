package protein.mechanism

import scala.collection.mutable

case class ProteinModifications(mods: Map[Site, SiteState]) extends AnyVal {

  def get(s: Site): Option[SiteState] = mods.get(s)

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

  override def toString = mods.iterator.map({ case (s, st) => s"$s~$st" }).mkString("(", ",", ")")
}

object ProteinModifications {
  def noModifications: ProteinModifications = ProteinModifications(Map())
}