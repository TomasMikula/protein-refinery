package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.{Discrete, Dom, IncRefSet, Propagation}
import nutcracker.syntax.dom._
import nutcracker.util.{ContU, DeepEqual, DeepEqualK, EqualK, FreeObjectOutput, IsEqual, MonadObjectOutput, ShowK}
import nutcracker.util.ops._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.util.{Unification, mapUnion}
import proteinrefinery.util.Unification.Syntax._

import scalaz.Id.Id
import scalaz.{Applicative, Equal, Monad, State, StateT}
import scalaz.std.tuple._
import scalaz.syntax.equal._
import scalaz.syntax.monad._

case class AgentsPattern[Ref[_]](
  agents: Vector[Option[ProteinPattern[Ref]]],
  bonds: Vector[Option[(AgentIndex, LocalSiteId[Ref], AgentIndex, LocalSiteId[Ref])]],
  assocs: Vector[Option[(AgentIndex, AgentIndex, Ref[IncRefSet[Ref, Discrete[Assoc[Ref]]]])]],
  unbound: List[(AgentIndex, LocalSiteId[Ref])]
) {
  import AgentsPattern._

  lazy val isAdmissible: Boolean = {
    agents forall (_ forall (_.isAdmissible))
    // TODO admissibility of bonds
  }

  def apply(i: AgentIndex): ProteinPattern[Ref] = agents(i.value).get

  def agentIterator: Iterator[ProteinPattern[Ref]] = agents.iterator.mapFilter(identity)

  def modify(a: Action[Ref])(implicit ev: EqualK[Ref]): AgentsPattern[Ref] = a match {
    case Link(i, si, j, sj) => link0(i, si, j, sj)._1
    case Unlink(id) => unlink(id)
    case Modify(i, rmMods, addMods, enzyme) =>
      modifyAgent(i, rmMods, addMods)
    case Replace(from, to, insert) =>
      ???
  }

  def addAgent(a: ProteinPattern[Ref]): (AgentsPattern[Ref], AgentIndex) =
    (copy(agents = agents :+ Some(a)), AgentIndex(agents.size))

  def removeAgent(i: AgentIndex): AgentsPattern[Ref] = ???

  def updateAgent(i: AgentIndex, u: ProteinPattern.Update[Ref])(implicit ev: EqualK[Ref]): Option[(AgentsPattern[Ref], AgentsPattern.Delta[Ref])] =
    (this(i): ProteinPattern[Ref]).update(u) match {
      case Some((pp, δ)) => Some((copy(agents = agents.updated(i.value, Some(pp))), Delta.update(i, δ)))
      case None => None
    }

  def requireUnbound(i: AgentIndex, s: SiteLabel)(implicit ev: EqualK[Ref]): AgentsPattern[Ref] =
    requireUnbound0(i, LocalSiteId(s))

  def requireUnbound0(i: AgentIndex, s: LocalSiteId[Ref])(implicit ev: EqualK[Ref]): AgentsPattern[Ref] = {
    require(hasAgent(i.value))
    require(isNotBound(i, s))
    AgentsPattern(agents, bonds, assocs, (i, s) :: unbound)
  }

  def link(i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel)(implicit ev: EqualK[Ref]): (AgentsPattern[Ref], LinkId) =
    link0(i, LocalSiteId(si), j, LocalSiteId(sj))

  def link0(i: AgentIndex, si: LocalSiteId[Ref], j: AgentIndex, sj: LocalSiteId[Ref])(implicit ev: EqualK[Ref]): (AgentsPattern[Ref], LinkId) = {
    require(hasAgent(i.value))
    require(hasAgent(j.value))
    require(isUnbound(i, si))
    require(isUnbound(j, sj))
    (AgentsPattern(agents, bonds :+ Some((i, si, j, sj)), assocs, unbound.filter(u => u =/= ((i, si)) && u =/= ((j, sj)))), LinkId(bonds.size))
  }

  def unlink(id: LinkId): AgentsPattern[Ref] = {
    require(hasBond(id.value))
    val Some((i, si, j, sj)) = bonds(id.value)
    AgentsPattern(agents, bonds.updated(id.value, None), assocs, (i, si) :: (j, sj) :: unbound)
  }

  def modifyAgent(i: AgentIndex, rmMods: ProteinModifications[Ref], addMods: ProteinModifications[Ref]): AgentsPattern[Ref] = {
    require(hasAgent(i.value))
    updateAgent(i, _.modify(rmMods, addMods))
  }

  def updateAgent(i: AgentIndex, f: ProteinPattern[Ref] => ProteinPattern[Ref]): AgentsPattern[Ref] = {
    val ag = f(agents(i.value).get)
    AgentsPattern(agents.updated(i.value, Some(ag)), bonds, assocs, unbound)
  }

  def addAssoc(i: AgentIndex, j: AgentIndex, assocRef: Ref[IncRefSet[Ref, Discrete[Assoc[Ref]]]]): (AgentsPattern[Ref], AssocId) = {
    val assoc = (i, j, assocRef)
    (AgentsPattern[Ref](agents, bonds, assocs :+ Some(assoc), unbound), AssocId(assocs.size))
  }

  def getBond(id: LinkId): Option[(ProteinPattern[Ref], LocalSiteId[Ref], ProteinPattern[Ref], LocalSiteId[Ref])] =
    bonds(id.value).map(reifyBond)

  def getBonds: List[(ProteinPattern[Ref], LocalSiteId[Ref], ProteinPattern[Ref], LocalSiteId[Ref])] =
    bonds.iterator.collectToList(_.map(reifyBond))

  def getUnbound: List[(ProteinPattern[Ref], LocalSiteId[Ref])] =
    unbound map { case (i, s) => (apply(i), s) }

  def partition(that: AgentsPattern[Ref]): (Option[AgentsPattern[Ref]], Option[AgentsPattern[Ref]], Option[AgentsPattern[Ref]]) = ???

  override def toString: String =
    show[FreeObjectOutput[String, Ref, ?]].showShallow(ShowK.fromToString[Ref])

  def show[F[_]](implicit F: MonadObjectOutput[F, String, Ref]): F[Unit] = {
    val bondsByAgent = bonds.iterator.zipWithIndex.mapFilter({ case (l, i) => l.map((_, i)) }).flatMap[(AgentIndex, (LocalSiteId[Ref], Either[Unbound.type , LinkId]))]{
      case ((pi, ps, qi, qs), linkIdx) =>
        Iterator((pi, (ps, Right(LinkId(linkIdx)))), (qi, (qs, Right(LinkId(linkIdx)))))
    }
    val nonBondsByAgent = unbound.iterator.map[(AgentIndex, (LocalSiteId[Ref], Either[Unbound.type , LinkId]))]{
      case (i, s) => (i, (s, Left(Unbound)))
    }
    val linksByAgent = (bondsByAgent ++ nonBondsByAgent).toMultiMap[AgentIndex, (LocalSiteId[Ref], Either[Unbound.type , LinkId])]

    agents.iterator.zipWithIndex.mapFilter({ case (pp, i) => pp.map(pp =>
      pp.showWithBonds[F](linksByAgent.getOrElse(AgentIndex(i), Nil).toMap)
    )}).intersperse(F.write(", ")).foldRight[F[Unit]](F.point(()))((fu, acc) => fu >> acc)
  }

  def showAssocs[F[_]](implicit F: MonadObjectOutput[F, String, Ref]): F[Unit] = {
    assocs.iterator.mapFilter(identity).map(ijs => {
      val (i, j, sols) = ijs
      for {
        _ <- F.write(s"agent ${i.value} has to be associated with agent ${j.value}, with solutions: ")
        u <- F.writeObject(sols)
      } yield u
    }).sequence_
  }

  private def reifyBond(b: (AgentIndex, LocalSiteId[Ref], AgentIndex, LocalSiteId[Ref])): (ProteinPattern[Ref], LocalSiteId[Ref], ProteinPattern[Ref], LocalSiteId[Ref]) = b match {
    case (i, si, j, sj) => (apply(i), si, apply(j), sj)
  }

  @inline private def hasAgent(i: Int): Boolean =
    i >= 0 && i < agents.size && agents(i).isDefined

  @inline private def getAgent(i: Int): Option[ProteinPattern[Ref]] =
    if(i >= 0 & i < agents.size) agents(i)
    else None

  @inline private def hasBond(i: Int): Boolean =
    i >= 0 && i < bonds.size && bonds(i).isDefined

  @inline private def isUnbound(i: AgentIndex, s: LocalSiteId[Ref]): Boolean =
    unbound.contains((i, s))

  @inline private def isNotBound(i: AgentIndex, s: LocalSiteId[Ref])(implicit ev: EqualK[Ref]): Boolean =
    bonds.forall(_ match {
      case Some((p, ps, q, qs)) => (p =/= i || ps =/= s) && (q =/= i || qs =/= s)
      case None => true
    })
}

object AgentsPattern {

  type Update[Ref[_]] = (AgentIndex, ProteinPattern.Update[Ref])

  case class Delta[Ref[_]](newAgents: Map[AgentIndex, ProteinPattern[Ref]], agentDeltas: Map[AgentIndex, ProteinPattern.Delta[Ref]]) {
    def ifNonEmpty: Option[Delta[Ref]] =
      if(newAgents.nonEmpty || agentDeltas.nonEmpty) Some(this)
      else None
  }

  object Delta {
    def update[Ref[_]](i: AgentIndex, d: ProteinPattern.Delta[Ref]): Delta[Ref] =
      Delta(Map(), Map(i -> d))

    def newAgent[Ref[_]](i: AgentIndex, a: ProteinPattern[Ref]): Delta[Ref] =
      Delta(Map(i -> a), Map())
  }

  def empty[Ref[_]]: AgentsPattern[Ref] =
    AgentsPattern(Vector.empty, Vector.empty, Vector.empty, Nil)

  def addAgent[Ref[_]](a: ProteinPattern[Ref]): State[AgentsPattern[Ref], AgentIndex] =
    State(_.addAgent(a))

  def removeAgent[Ref[_]](i: AgentIndex): State[AgentsPattern[Ref], Unit] =
    State(s => (s.removeAgent(i), ()))

  def requireUnbound0[Ref[_]](i: AgentIndex, site: LocalSiteId[Ref])(implicit ev: EqualK[Ref]): State[AgentsPattern[Ref], Unit] =
    State(s => (s.requireUnbound0(i, site), ()))

  def requireUnbound[Ref[_]](i: AgentIndex, site: SiteLabel)(implicit ev: EqualK[Ref]): State[AgentsPattern[Ref], Unit] =
    State(s => (s.requireUnbound(i, site), ()))

  def addLink[Ref[_]](i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel)(implicit ev: EqualK[Ref]): State[AgentsPattern[Ref], LinkId] =
    State(_.link(i, si, j, sj))

  def removeLink[Ref[_]](id: LinkId): State[AgentsPattern[Ref], Unit] =
    State(s => (s.unlink(id), ()))

  implicit def deepEqualKInstance: DeepEqualK[AgentsPattern, AgentsPattern] = new DeepEqualK[AgentsPattern, AgentsPattern] {
    def equal[Ptr1[_], Ptr2[_]](ap1: AgentsPattern[Ptr1], ap2: AgentsPattern[Ptr2]): IsEqual[Ptr1, Ptr2] = {
      val AgentsPattern(ags1, bnds1, ascs1, unb1) = ap1
      val AgentsPattern(ags2, bnds2, ascs2, unb2) = ap2
      IsEqual[Ptr1, Ptr2].equal(ags1, ags2) && IsEqual(bnds1, bnds2) && IsEqual(ascs1, ascs2) && IsEqual(unb1, unb2)
    }
  }

  implicit def domInstance[Ref[_]](implicit ev: EqualK[Ref]): Dom.Aux[AgentsPattern[Ref], Update[Ref], Delta[Ref]] = new Dom[AgentsPattern[Ref]] {
    type Update = AgentsPattern.Update[Ref]
    type Delta = AgentsPattern.Delta[Ref]

    def update(ap: AgentsPattern[Ref], u: Update): Option[(AgentsPattern[Ref], Delta)] =
      ap.updateAgent(u._1, u._2)

    def combineDeltas(d1: Delta, d2: Delta): Delta = {
      val newAgents = d1.newAgents ++ d2.newAgents
      val deltas = mapUnion[AgentIndex, ProteinPattern.Delta[Ref], Id](d1.agentDeltas, d2.agentDeltas)((δ1, δ2) => {
        Dom[ProteinPattern[Ref]].combineDeltas(δ1, δ2)
      })
      Delta(newAgents, deltas)
    }

    def assess(ap: AgentsPattern[Ref]): Dom.Status[Update] =
      if(ap.isAdmissible) Dom.Refined
      else Dom.Failed
  }

  implicit def unificationInstance[Ref[_]](implicit ev: EqualK[Ref]): Unification.Aux[AgentsPattern[Ref], Update[Ref], Delta[Ref]] = new Unification[AgentsPattern[Ref]] {
    type Update = AgentsPattern.Update[Ref]
    type Delta = AgentsPattern.Delta[Ref]

    def unify(ap1: AgentsPattern[Ref], ap2: AgentsPattern[Ref]): (Option[Delta], AgentsPattern[Ref], Option[Delta]) = {
      val agents = Vector.newBuilder[Option[ProteinPattern[Ref]]]
      val newAgs1 = Map.newBuilder[AgentIndex, ProteinPattern[Ref]]
      val newAgs2 = Map.newBuilder[AgentIndex, ProteinPattern[Ref]]
      val deltas1 = Map.newBuilder[AgentIndex, ProteinPattern.Delta[Ref]]
      val deltas2 = Map.newBuilder[AgentIndex, ProteinPattern.Delta[Ref]]

      val n = math.max(ap1.agents.size, ap2.agents.size)
      for(i <- 0 until n) {
        val a1 = ap1.getAgent(i)
        val a2 = ap2.getAgent(i)

        (a1, a2) match {
          case (None, None) =>
            agents += None
          case (Some(a1), None) =>
            agents += Some(a1)
            newAgs2 += ((AgentIndex(i), a1))
          case (None, Some(a2)) =>
            agents += Some(a2)
            newAgs1 += ((AgentIndex(i), a2))
          case (Some(a1), Some(a2)) =>
            val (d1, pp, d2) = a1 unify a2
            agents += Some(pp)
            d1.foreach(d => deltas1 += ((AgentIndex(i), d)))
            d2.foreach(d => deltas2 += ((AgentIndex(i), d)))
        }
      }

      val bonds = (ap1.bonds ++ ap2.bonds).distinct
      val assocs = (ap1.assocs ++ ap2.assocs).distinct // XXX will always be distinct, since the Assoc Refs will be distinct
      val unbound = (ap1.unbound ++ ap2.unbound).distinct

      val delta1 = Delta(newAgs1.result(), deltas1.result())
      val delta2 = Delta(newAgs2.result(), deltas2.result())

      (delta1.ifNonEmpty, AgentsPattern(agents.result(), bonds, assocs, unbound), delta2.ifNonEmpty)
    }

    def dom: Dom.Aux[AgentsPattern[Ref], Update, Delta] = domInstance
  }

  trait Ops[M[_], Ref[_]] {
    implicit def Propagation: Propagation[M, Ref]

    def IncRefSets: nutcracker.IncRefSets[M, Ref]
    def AssocSearch: Assoc.Search[M, Ref]

    def requireAssoc(i: AgentIndex, j: AgentIndex, predicate: Assoc[Ref] => Boolean)(implicit M: Monad[M], ev: EqualK[Ref]): StateT[M, AgentsPattern[Ref], AssocId] = {
      StateT(ap => {
        val assocC = Discrete.filterMap(AssocSearch.assocC(ap(i).protein, ap(j).protein)) { a =>
          if (predicate(a)) Some(a)
          else None
        }
        val assocS = IncRefSets.collect(assocC)
        M.map(assocS)(ref => ap.addAssoc(i, j, ref))
      })
    }

    def forEachAssoc(ap: AgentsPattern[Ref])(implicit M: Applicative[M]): ContU[M, Assoc.Ref[Ref]] = {
      val as = ap.assocs.flatMap {
        case Some((_, _, asr)) => Vector(asr)
        case None => Vector()
      }
      ContU.sequence(as.map(asr => IncRefSets.forEach(asr)))
    }
  }
}

final case class AgentIndex(value: Int) extends AnyVal
object AgentIndex {
  implicit val equalInstance: Equal[AgentIndex] = new Equal[AgentIndex] {
    def equal(i: AgentIndex, j: AgentIndex): Boolean = i.value == j.value
  }

  implicit def deepEqualInstance[Ptr1[_], Ptr2[_]]: DeepEqual[AgentIndex, AgentIndex, Ptr1, Ptr2] =
    new DeepEqual[AgentIndex, AgentIndex, Ptr1, Ptr2] {
      def equal(i: AgentIndex, j: AgentIndex): IsEqual[Ptr1, Ptr2] = IsEqual(i.value == j.value)
    }
}

final case class LinkId(value: Int) extends AnyVal
object LinkId {
  implicit def equalInstance: Equal[LinkId] = new Equal[LinkId] {
    def equal(a1: LinkId, a2: LinkId): Boolean = a1.value == a2.value
  }
  implicit def deepEqualInstance[Ptr1[_], Ptr2[_]]: DeepEqual[LinkId, LinkId, Ptr1, Ptr2] =
    new DeepEqual[LinkId, LinkId, Ptr1, Ptr2] {
      def equal(a1: LinkId, a2: LinkId): IsEqual[Ptr1, Ptr2] = (a1, a2) match {
        case (LinkId(i1), LinkId(i2)) => IsEqual(i1 == i2)
      }
    }
}

final case class AssocId(value: Int) extends AnyVal
object AssocId {
  implicit def equalInstance: Equal[AssocId] = new Equal[AssocId] {
    def equal(i: AssocId, j: AssocId): Boolean = i.value == j.value
  }
}

object Unbound {
  implicit def equalInstance: Equal[Unbound.type] = new Equal[Unbound.type] {
    def equal(a1: Unbound.type, a2: Unbound.type): Boolean = true
  }
}

sealed abstract class Action[Ref[_]]
case class Link[Ref[_]](i1: AgentIndex, s1: LocalSiteId[Ref], i2: AgentIndex, s2: LocalSiteId[Ref]) extends Action[Ref] {
  def flip: Link[Ref] = Link(i2, s2, i1, s1)
}
case class Unlink[Ref[_]](id: LinkId) extends Action[Ref]
case class Modify[Ref[_]](i: AgentIndex, rm: ProteinModifications[Ref], add: ProteinModifications[Ref], enzyme: Option[AgentIndex]) extends Action[Ref]
case class Replace[Ref[_]](from: AgentIndex, to: AgentIndex, insert: List[ProteinPattern[Ref]]) extends Action[Ref]

object Link {
  def apply[Ref[_]](i1: AgentIndex, s1: Site.Definite, i2: AgentIndex, s2: Site.Definite): Link[Ref] =
    Link(i1, LocalSiteId[Ref](s1), i2, LocalSiteId[Ref](s2))
}

object Action {
  implicit val deepEqualKInstance: DeepEqualK[Action, Action] = new DeepEqualK[Action, Action] {
    def equal[Ptr1[_], Ptr2[_]](a1: Action[Ptr1], a2: Action[Ptr2]): IsEqual[Ptr1, Ptr2] =
      (a1, a2) match {
        case (Link(i1, si1, j1, sj1), Link(i2, si2, j2, sj2)) =>
          IsEqual[Ptr1, Ptr2].equal(i1, i2) && IsEqual(si1, si2) && IsEqual(j1, j2) && IsEqual(sj1, sj2)
        case (Unlink(l1), Unlink(l2)) =>
          IsEqual(l1, l2)
        case (Modify(i1, rm1, add1, enz1), Modify(i2, rm2, add2, enz2)) =>
          IsEqual[Ptr1, Ptr2].equal(i1, i2) && IsEqual(rm1, rm2) && IsEqual(add1, add2) && IsEqual(enz1, enz2)
        case (Replace(f1, t1, ins1), Replace(f2, t2, ins2)) =>
          IsEqual[Ptr1, Ptr2].equal(f1, f2) && IsEqual(t1, t2) && IsEqual(ins1, ins2)
        case _ =>
          IsEqual(false)
      }
  }
}