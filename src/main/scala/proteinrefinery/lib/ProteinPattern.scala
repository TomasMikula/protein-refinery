package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Promise.{Completed, Conflict, Empty}
import nutcracker.{Discrete, Dom, Promise, UpdateResult}
import nutcracker.util.ops.iterator._
import nutcracker.syntax.dom._
import nutcracker.util.{DeepEqualK, DeepShowK, EqualK, FreeObjectOutput, IsEqual, MonadObjectOutput, ShowK}
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{Identification, Unification}

import scalaz.{Semigroup, \&/}
import scalaz.std.either._
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.show._

case class ProteinPattern[Ref[_]](protein: Protein, mods: ProteinModifications[Ref]) {

  def isAdmissible = mods.isAdmissible

  def isCompatibleWith(that: ProteinPattern[Ref]): Boolean =
    (this.protein == that.protein) && this.isAdmissible && that.isAdmissible && (this.mods combine that.mods).isAdmissible

  def addModification(site: SiteLabel, state: SiteState): ProteinPattern[Ref] =
    addModification(ISite[Ref](site), state)

  def addModification(site: ISite[Ref], state: SiteState): ProteinPattern[Ref] =
    ProteinPattern(protein, mods.addModification(site, state))

  def modify(rmMods: ProteinModifications[Ref], addMods: ProteinModifications[Ref]): ProteinPattern[Ref] =
    ProteinPattern(protein, this.mods minus rmMods combine addMods)

  def mentionedSites: Set[LocalSiteId[Ref]] = mods.mentionedSites

  override def toString: String = show[FreeObjectOutput[String, Ref, ?]].showShallow(ShowK.fromToString)

  def show[M[_]](implicit M: MonadObjectOutput[M, String, Ref]): M[Unit] = showWithBonds[M](Map())

  def showWithBonds[M[_]](bonds: Map[LocalSiteId[Ref], Either[Unbound.type, LinkId]])(implicit M: MonadObjectOutput[M, String, Ref]): M[Unit] = {
    type LinkDesc = Either[Unbound.type, LinkId]
    type LinkDom = Promise[LinkDesc]
    type SiteAttr = (LinkDom, SiteState)

    val bonds1: List[(ISite[Ref], SiteAttr)] =
      bonds.foldLeft[List[(ISite[Ref], SiteAttr)]](Nil)((l, siteIdLink) => {
        val (siteId, link) = siteIdLink
        val siteDesc: ISite[Ref] = ISite(siteId)
        val linkDom = Promise.completed(link)
        (siteDesc, (linkDom, Promise.empty)) :: l
      })

    val mods1 = mods.mods.inject(x => (x.site, (Promise.empty[LinkDesc], x.state)))

    implicit val siteAttrUnif =
      Unification.tuple2[Promise[LinkDesc], SiteState](
        Unification.promiseUnification[LinkDesc],
        SiteState.unificationInstance)
    implicit val unif =
      Unification.tuple2[ISite[Ref], SiteAttr]
    implicit val ident =
      Identification.by[(ISite[Ref], SiteAttr), ISite[Ref]](_._1)(unif, ISite.identificationInstance)

    val bag = mods1.addAll(bonds1)

    def siteStr(s: (ISite[Ref], SiteAttr)): M[Unit] = {
      val (site, (link, state)) = s
      val siteDesc = site.show[M]
      val stateDesc = state match {
        case Empty => ""
        case Completed(label) => "~" + label.shows
        case Conflict => "~⊥"
      }
      val linkStr = link match {
        case Empty => "?"
        case Completed(lnk) => lnk match {
          case Left(_) => ""
          case Right(lnkId) => "!" + lnkId.value
        }
        case Conflict => "!⊥"
      }
      for {
        _ <- siteDesc
        _ <- M.write(stateDesc)
        u <- M.write(linkStr)
      } yield u
    }

    val sites = bag.list.iterator.map(siteStr).intersperse(M.write(","))

    for {
      _ <- M.write(protein.shows)
      _ <- M.write("(")
      _ <- sites.foldRight(M.point(()))((m, acc) => m >> acc)
      u <- M.write(")")
    } yield u
  }
}

object ProteinPattern {
  type Update[Var[_]] = ProteinModifications.Update[Var]
  type Delta[Var[_]] = Protein.Delta \&/ ProteinModifications.Delta[Var]

  type Ref[Var[_]] = Var[Discrete[ProteinPattern[Var]]]

  def apply[Var[_]](p: Protein): ProteinPattern[Var] =
    ProteinPattern(p, ProteinModifications.noModifications)

  implicit def domInstance[Var[_]](implicit ev: EqualK[Var]): Dom.Aux[ProteinPattern[Var], Update[Var], Delta[Var]] =
    new Dom[ProteinPattern[Var]] {
      type Update = ProteinPattern.Update[Var]
      type Delta = ProteinPattern.Delta[Var]

      def update[P <: ProteinPattern[Var]](d: P, u: Update): UpdateResult[ProteinPattern[Var], IDelta, P] = {
        val ProteinPattern(p, mods) = d
        mods.update(u).map(mods => ProteinPattern(p, mods), delta => \&/.That(delta))
      }

      override def deltaSemigroup: Semigroup[Delta] =
        \&/.TheseSemigroup(Protein.deltaSemigroup, ProteinModifications.domInstance.deltaSemigroup)

      def appendDeltas(d1: Delta, d2: Delta): Delta = deltaSemigroup.append(d1, d2)

      override def isFailed(d: ProteinPattern[Var]): Boolean = d.mods.isFailed
    }

  implicit def unificationInstance[Var[_]](implicit ev: EqualK[Var]): Unification.Aux[ProteinPattern[Var], Update[Var], Delta[Var]] =
    new Unification[ProteinPattern[Var]] {
      type Update = ProteinPattern.Update[Var]
      type Delta = ProteinPattern.Delta[Var]

      def unify(pp1: ProteinPattern[Var], pp2: ProteinPattern[Var]): (Option[Delta], ProteinPattern[Var], Option[Delta]) = {
        import Unification.Syntax._
        val (pd1, p, pd2) = Protein.unify(pp1.protein, pp2.protein)
        val (d1, mods, d2) = pp1.mods unify pp2.mods
        (theseOpt(pd1, d1), ProteinPattern(p, mods), theseOpt(pd2, d2))
      }

      def dom: Dom.Aux[ProteinPattern[Var], Update, Delta] = domInstance

      private def theseOpt[A, B](a: Option[A], b: Option[B]): Option[A \&/ B] =
        (a, b) match {
          case (Some(a), Some(b)) => Some(\&/.Both(a, b))
          case (Some(a), None) => Some(\&/.This(a))
          case (None, Some(b)) => Some(\&/.That(b))
          case (None, None) => None
        }
    }

  implicit val deepEqualKInstance: DeepEqualK[ProteinPattern, ProteinPattern] =
    new DeepEqualK[ProteinPattern, ProteinPattern] {
      def equal[Ptr1[_], Ptr2[_]](pp1: ProteinPattern[Ptr1], pp2: ProteinPattern[Ptr2]): IsEqual[Ptr1, Ptr2] =
        IsEqual[Ptr1, Ptr2].equal(pp1.protein, pp2.protein) && IsEqual(pp1.mods, pp2.mods)
    }

  implicit def deepShowKInstance: DeepShowK[ProteinPattern] =
    new DeepShowK[ProteinPattern] {
      def show[Ptr[_], M[_]](pp: ProteinPattern[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
        pp.show[M]
    }
}