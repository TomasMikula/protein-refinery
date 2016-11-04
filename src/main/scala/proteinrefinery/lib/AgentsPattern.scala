package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.Dom
import nutcracker.syntax.dom._
import nutcracker.util.EqualK
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.util.{Unification, mapUnion}
import proteinrefinery.util.Unification.Syntax._
import proteinrefinery.util.syntax._

import scalaz.Id.Id
import scalaz.{Equal, State}

case class AgentsPattern[Ref[_]](
  agents: Vector[Option[ProteinPattern[Ref]]],
  bonds: Vector[Option[(AgentIndex, LocalSiteId[Ref], AgentIndex, LocalSiteId[Ref])]],
  unbound: List[(AgentIndex, LocalSiteId[Ref])]
) {
  import AgentsPattern._

  lazy val isAdmissible: Boolean = {
    agents forall (_ forall (_.isAdmissible))
    // TODO admissibility of bonds
  }

  def apply(i: AgentIndex): ProteinPattern[Ref] = agents(i.value).get

  def agentIterator: Iterator[ProteinPattern[Ref]] = agents.iterator.mapFilter(identity)

  def modify(a: Action[Ref]): AgentsPattern[Ref] = a match {
    case Link(i, si, j, sj) => link0(i, si, j, sj)._1
    case Unlink(id) => unlink(id)
    case Modify(i, rmMods, addMods) =>
      ???
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

  def requireUnbound(i: AgentIndex, s: SiteLabel): AgentsPattern[Ref] =
    requireUnbound0(i, LocalSiteId(s))

  def requireUnbound0(i: AgentIndex, s: LocalSiteId[Ref]): AgentsPattern[Ref] = {
    require(hasAgent(i.value))
    require(isNotBound(i, s))
    AgentsPattern(agents, bonds, (i, s) :: unbound)
  }

  def link(i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel): (AgentsPattern[Ref], LinkId) =
    link0(i, LocalSiteId(si), j, LocalSiteId(sj))

  def link0(i: AgentIndex, si: LocalSiteId[Ref], j: AgentIndex, sj: LocalSiteId[Ref]): (AgentsPattern[Ref], LinkId) = {
    require(hasAgent(i.value))
    require(hasAgent(j.value))
    require(isUnbound(i, si))
    require(isUnbound(j, sj))
    (AgentsPattern(agents, bonds :+ Some((i, si, j, sj)), unbound.filter(u => u != ((i, si)) && u != ((j, sj)))), LinkId(bonds.size))
  }

  def unlink(id: LinkId): AgentsPattern[Ref] = {
    require(hasBond(id.value))
    val Some((i, si, j, sj)) = bonds(id.value)
    AgentsPattern(agents, bonds.updated(id.value, None), (i, si) :: (j, sj) :: unbound)
  }

  def getBond(id: LinkId): Option[(ProteinPattern[Ref], LocalSiteId[Ref], ProteinPattern[Ref], LocalSiteId[Ref])] =
    bonds(id.value).map(reifyBond)

  def getBonds: List[(ProteinPattern[Ref], LocalSiteId[Ref], ProteinPattern[Ref], LocalSiteId[Ref])] =
    bonds.iterator.collectToList(_.map(reifyBond))

  def getUnbound: List[(ProteinPattern[Ref], LocalSiteId[Ref])] =
    unbound map { case (i, s) => (apply(i), s) }

  def partition(that: AgentsPattern[Ref]): (Option[AgentsPattern[Ref]], Option[AgentsPattern[Ref]], Option[AgentsPattern[Ref]]) = ???

  override def toString: String = {
    val bondsByAgent = bonds.iterator.zipWithIndex.mapFilter({ case (l, i) => l.map((_, i)) }).flatMap[(AgentIndex, (LocalSiteId[Ref], Either[Unbound.type , LinkId]))]{
      case ((pi, ps, qi, qs), linkIdx) =>
        Iterator((pi, (ps, Right(LinkId(linkIdx)))), (qi, (qs, Right(LinkId(linkIdx)))))
    }
    val nonBondsByAgent = unbound.iterator.map[(AgentIndex, (LocalSiteId[Ref], Either[Unbound.type , LinkId]))]{
      case (i, s) => (i, (s, Left(Unbound)))
    }
    val linksByAgent = (bondsByAgent ++ nonBondsByAgent).toMultiMap[AgentIndex, (LocalSiteId[Ref], Either[Unbound.type , LinkId])]

    agents.iterator.zipWithIndex.mapFilter({ case (pp, i) => pp.map(pp =>
      pp.toString(linksByAgent.getOrElse(AgentIndex(i), Nil).toMap)
    )}).mkString(", ")
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

  @inline private def isNotBound(i: AgentIndex, s: LocalSiteId[Ref]): Boolean =
    bonds.forall(_ match {
      case Some((p, ps, q, qs)) => (p != i || ps != s) && (q != i || qs != s)
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
    AgentsPattern(Vector.empty, Vector.empty, Nil)

  def addAgent[Ref[_]](a: ProteinPattern[Ref]): State[AgentsPattern[Ref], AgentIndex] =
    State(_.addAgent(a))

  def removeAgent[Ref[_]](i: AgentIndex): State[AgentsPattern[Ref], Unit] =
    State(s => (s.removeAgent(i), ()))

  def requireUnbound0[Ref[_]](i: AgentIndex, site: LocalSiteId[Ref]): State[AgentsPattern[Ref], Unit] =
    State(s => (s.requireUnbound0(i, site), ()))

  def requireUnbound[Ref[_]](i: AgentIndex, site: SiteLabel): State[AgentsPattern[Ref], Unit] =
    State(s => (s.requireUnbound(i, site), ()))

  def addLink[Ref[_]](i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel): State[AgentsPattern[Ref], LinkId] =
    State(_.link(i, si, j, sj))

  def removeLink[Ref[_]](id: LinkId): State[AgentsPattern[Ref], Unit] =
    State(s => (s.unlink(id), ()))

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
      val unbound = (ap1.unbound ++ ap2.unbound).distinct

      val delta1 = Delta(newAgs1.result(), deltas1.result())
      val delta2 = Delta(newAgs2.result(), deltas2.result())

      (delta1.ifNonEmpty, AgentsPattern(agents.result(), bonds, unbound), delta2.ifNonEmpty)
    }

    def dom: Dom.Aux[AgentsPattern[Ref], Update, Delta] = domInstance
  }
}

final case class AgentIndex(value: Int) extends AnyVal

final case class LinkId(value: Int) extends AnyVal
object LinkId {
  implicit def equalInstance: Equal[LinkId] = new Equal[LinkId] {
    def equal(a1: LinkId, a2: LinkId): Boolean = a1.value == a2.value
  }
}

object Unbound {
  implicit def equalInstance: Equal[Unbound.type] = new Equal[Unbound.type] {
    def equal(a1: Unbound.type, a2: Unbound.type): Boolean = true
  }
}

sealed abstract class Action[Ref[_]]
case class Link[Ref[_]](i1: AgentIndex, s1: LocalSiteId[Ref], i2: AgentIndex, s2: LocalSiteId[Ref]) extends Action[Ref]
case class Unlink[Ref[_]](id: LinkId) extends Action[Ref]
case class Modify[Ref[_]](i: AgentIndex, rm: ProteinModifications[Ref], add: ProteinModifications[Ref]) extends Action[Ref]
case class Replace[Ref[_]](from: AgentIndex, to: AgentIndex, insert: List[ProteinPattern[Ref]]) extends Action[Ref]

object Link {
  def apply[Ref[_]](i1: AgentIndex, s1: Site.Definite, i2: AgentIndex, s2: Site.Definite): Link[Ref] =
    Link(i1, LocalSiteId(s1), i2, LocalSiteId(s2))
}