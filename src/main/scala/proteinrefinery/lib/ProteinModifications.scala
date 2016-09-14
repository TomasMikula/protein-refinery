package proteinrefinery.lib

import nutcracker.Dom.Status
import nutcracker.{Antichain, Dom, Final, Join, JoinDom}
import proteinrefinery.lib.AdmissibleProteinModifications.{FinalSiteModifications, NonFinalSiteModifications}

import scala.language.higherKinds
import scalaz.std.option._
import proteinrefinery.util.{buildMap, mapIntersect, mapSplit, mapUnion}

sealed trait ProteinModifications {

  def isAdmissible: Boolean = this match {
    case AdmissibleProteinModifications(_, _) => true
    case InvalidProteinModifications => false
  }

  /** `ProteinModifications` is isomorphic to `Option[AdmissibleProteinModifications]` */
  def toOption: Option[AdmissibleProteinModifications] = this match {
    case a @ AdmissibleProteinModifications(_, _) => Some(a)
    case InvalidProteinModifications => None
  }

  final def addModification(site: Site, state: SiteState): ProteinModifications =
    addModification0(Antichain(site), state)

  def addModification0(site: Site.Dom, state: SiteState): ProteinModifications

  def combine(that: ProteinModifications): ProteinModifications = (this, that) match {
    case (a @ AdmissibleProteinModifications(_, _), b @ AdmissibleProteinModifications(_, _)) => a.combine0(b)
    case _ => InvalidProteinModifications
  }
}

case object InvalidProteinModifications extends ProteinModifications {
  def addModification0(site: Site.Dom, state: SiteState): ProteinModifications = this
}

case class AdmissibleProteinModifications(
  nonFinalSiteMods: NonFinalSiteModifications,
  finalSiteMods: FinalSiteModifications
) extends ProteinModifications {

  def addModification0(site: Site.Dom, state: SiteState): ProteinModifications =
    ProteinModifications.fromOption(finalSiteMods.addModification0(site, state).map(AdmissibleProteinModifications(nonFinalSiteMods, _)))

  def combine0(that: AdmissibleProteinModifications): ProteinModifications =
    ProteinModifications.fromOption(for {
      (nf, fin) <- (this.nonFinalSiteMods combine that.nonFinalSiteMods)
      fin <- FinalSiteModifications.combine(this.finalSiteMods, that.finalSiteMods, fin)
    } yield AdmissibleProteinModifications(nf, fin))

}

object AdmissibleProteinModifications {

  def apply(mods: Iterable[(Site, SiteState)]): AdmissibleProteinModifications =
    AdmissibleProteinModifications(
      NonFinalSiteModifications.noModifications,
      FinalSiteModifications(mods.iterator.map(ss => (Antichain(ss._1), (ss._2, Set[Site.Ref]()))).toMap)
    )

  private[AdmissibleProteinModifications]
  final case class FinalSiteModifications(mods: Map[Site.Dom, (SiteState, Set[Site.Ref])]) extends AnyVal {

    def addModification0(site: Site.Dom, state: SiteState): Option[FinalSiteModifications] =
      mods.get(site).fold[Option[FinalSiteModifications]](
        Some(FinalSiteModifications(mods.updated(site, (state, Set())))))(
        sr => if(sr._1 == state) Some(this) else None
      )

    def combine(that: FinalSiteModifications): Option[FinalSiteModifications] =
      mapUnion(this.mods, that.mods)((sr1, sr2) => {
        val (st1, refs1) = sr1
        val (st2, refs2) = sr2
        if(st1 == st2) Some((st1, refs1 union refs2))
        else None
      }).map(FinalSiteModifications(_))

    /** Returns modifications that are less specific than either `this` or `that`
      * and are the most specific such modifications. In other words, returns the
      * greatest lower bound of the two in the (partial) ordering given by specificity.
      */
    def meet(that: FinalSiteModifications): Map[Site.Dom, SiteState] = {
      mapIntersect(this.mods, that.mods)((sr1, sr2) => {
        val (st1, refs1) = sr1
        val (st2, refs2) = sr2
        if(st1 == st2) Some(st1) else None
      })
    }

    override def toString = mods.iterator.map({ case (s, st) => s"$s~$st" }).mkString("(", ",", ")")

  }

  object FinalSiteModifications {
    def noModifications: FinalSiteModifications = FinalSiteModifications(Map())

    def combine(modss: FinalSiteModifications*): Option[FinalSiteModifications] = modss.headOption match {
      case None => Some(FinalSiteModifications.noModifications)
      case Some(mods) => combine(modss.tail:_*).flatMap(mods combine _)
    }
  }

  private[AdmissibleProteinModifications]
  final case class NonFinalSiteModifications(mods: Map[Site.Ref, (Site.Dom, SiteState)]) extends AnyVal {

    def combine(that: NonFinalSiteModifications): Option[(NonFinalSiteModifications, FinalSiteModifications)] = {
      implicit val sDom: JoinDom[Site.Dom] = new JoinDom.Template[Site.Dom] {
        def ljoin0(d1: Site.Dom, d2: Site.Dom): Option[Site.Dom] = ???
        def assess(d: Site.Dom): Status[Site.Dom] = ???
      }
      implicit val stDom: JoinDom[SiteState] = new JoinDom.Template[SiteState] {
        def ljoin0(d1: SiteState, d2: SiteState): Option[SiteState] = ???
        def assess(d: SiteState): Status[SiteState] = ???
      }
      implicit val sFin: Final.Aux[Site.Dom, Site] = new Final[Site.Dom] {
        type Out = Site
        def embed(a: Site): Site.Dom = ???
        def extract(d: Site.Dom): Option[Site] = ???
      }

      for {
        m1 <- mapUnion(this.mods, that.mods)({ case ((s1, st1), (s2, st2)) =>
          val s = sDom.join(s1, s2)
          sDom.assess(s) match {
            case Dom.Failed => None
            case _ =>
              val st = stDom.join(st1, st2)
              stDom.assess(st) match {
                case Dom.Failed => None
                case _ =>
                  Some((s, st))
              }
          }
        })

        (nonFinMods, m2) = mapSplit(m1)({ case (s, st) =>
          if (sFin.isFinal(s)) Right((s, st))
          else Left((s, st))
        })

        finMods <- buildMap(m2.iterator.map({ case (ref, (s, st)) => (s, (st, Set(ref))) }))({ case ((st1, refs1), (st2, refs2)) =>
          val st = stDom.join(st1, st2)
          stDom.assess(st) match {
            case Dom.Failed => None
            case _ => Some((st, refs1 union refs2))
          }
        })
      } yield (NonFinalSiteModifications(nonFinMods), FinalSiteModifications(finMods))
    }

  }

  object NonFinalSiteModifications {
    def noModifications: NonFinalSiteModifications = NonFinalSiteModifications(Map())
  }

  def noModifications: AdmissibleProteinModifications =
    AdmissibleProteinModifications(
      NonFinalSiteModifications.noModifications,
      FinalSiteModifications.noModifications
    )
}

object ProteinModifications {
  def noModifications: AdmissibleProteinModifications = AdmissibleProteinModifications.noModifications

  def fromOption(pm: Option[AdmissibleProteinModifications]): ProteinModifications =
    pm.fold[ProteinModifications](InvalidProteinModifications)(x => x)

  implicit def domInstance: Dom.Aux[ProteinModifications, Join[ProteinModifications], Unit] =
    new Dom[ProteinModifications] {
      type Update = Join[ProteinModifications]
      type Delta = Unit
      override def assess(d: ProteinModifications): Dom.Status[Join[ProteinModifications]] = d match {
        case InvalidProteinModifications => Dom.Failed
        case AdmissibleProteinModifications(_, _) => Dom.Refined
      }

      override def update(d: ProteinModifications, u: Join[ProteinModifications]): Option[(ProteinModifications, Unit)] = {
        val res = d combine u.value
        if(res == d) None else Some((res, ()))
      }

      override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
    }
}
