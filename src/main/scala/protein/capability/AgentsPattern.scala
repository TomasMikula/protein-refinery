package protein.capability

import protein.mechanism.{ModifiedProtein, ProteinModifications, Site}
import protein.util.syntax._

import scalaz.State

case class AgentsPattern(
  agents: Vector[ProteinPattern],
  bonds: Vector[Option[(AgentIndex, Site, AgentIndex, Site)]],
  unbound: List[(AgentIndex, Site)]
) {

  def apply(i: AgentIndex): ProteinPattern = agents(i.value)

  def modify(a: Action): AgentsPattern = a match {
    case Link(i, si, j, sj) => link(i, si, j, sj)._1
    case Unlink(id) => unlink(id)
    case Modify(i, rmMods, addMods) =>
      ???
    case Replace(from, to, insert) =>
      ???
  }

  def addAgent(a: ProteinPattern): (AgentsPattern, AgentIndex) =
    (copy(agents = agents :+ a), AgentIndex(agents.size))

  def removeAgent(i: AgentIndex): AgentsPattern = ???

  def requireUnbound(i: AgentIndex, s: Site): AgentsPattern = {
    require(hasAgent(i.value))
    require(isNotBound(i, s))
    AgentsPattern(agents, bonds, (i, s) :: unbound)
  }

  def link(i: AgentIndex, si: Site, j: AgentIndex, sj: Site): (AgentsPattern, LinkId) = {
    require(hasAgent(i.value))
    require(hasAgent(j.value))
    require(isUnbound(i, si))
    require(isUnbound(j, sj))
    (AgentsPattern(agents, bonds :+ Some((i, si, j, sj)), unbound.filter(u => u != ((i, si)) && u != ((j, sj)))), LinkId(bonds.size))
  }

  def unlink(id: LinkId): AgentsPattern = {
    require(hasBond(id.value))
    val Some((i, si, j, sj)) = bonds(id.value)
    AgentsPattern(agents, bonds.updated(id.value, None), (i, si) :: (j, sj) :: unbound)
  }

  def getBond(id: LinkId): Option[(ProteinPattern, Site, ProteinPattern, Site)] =
    bonds(id.value).map(reifyBond)

  def getBonds: List[(ProteinPattern, Site, ProteinPattern, Site)] =
    bonds.iterator.collectToList(_.map(reifyBond))

  def getUnbound: List[(ProteinPattern, Site)] =
    unbound map { case (i, s) => (agents(i.value), s) }

  def unify(that: AgentsPattern): Option[AgentsPattern] = ???
  def partition(that: AgentsPattern): (Option[AgentsPattern], Option[AgentsPattern], Option[AgentsPattern]) = ???

  override def toString: String = {
    val bondsByAgent = bonds.iterator.zipWithIndex.mapFilter({ case (l, i) => l.map((_, i)) }).flatMap[(AgentIndex, (Site, Either[Unbound.type , LinkId]))]{
      case ((pi, ps, qi, qs), linkIdx) =>
        Iterator((pi, (ps, Right(LinkId(linkIdx)))), (qi, (qs, Right(LinkId(linkIdx)))))
    }
    val nonBondsByAgent = unbound.iterator.map[(AgentIndex, (Site, Either[Unbound.type , LinkId]))]{
      case (i, s) => (i, (s, Left(Unbound)))
    }
    val linksByAgent = (bondsByAgent ++ nonBondsByAgent).toMultiMap[AgentIndex, (Site, Either[Unbound.type , LinkId])]

    agents.iterator.zipWithIndex.map({ case (pp, i) =>
      pp.toString(linksByAgent.getOrElse(AgentIndex(i), Nil).toMap)
    }).mkString(", ")
  }

  private def reifyBond(b: (AgentIndex, Site, AgentIndex, Site)): (ProteinPattern, Site, ProteinPattern, Site) = b match {
    case (i, si, j, sj) => (agents(i.value), si, agents(j.value), sj)
  }

  @inline private def hasAgent(i: Int): Boolean =
    i >= 0 && i < agents.size && agents(i) != null

  @inline private def hasBond(i: Int): Boolean =
    i >= 0 && i < bonds.size && bonds(i).isDefined

  @inline private def isUnbound(i: AgentIndex, s: Site): Boolean =
    unbound.contains((i, s))

  @inline private def isNotBound(i: AgentIndex, s: Site): Boolean =
    bonds.forall(_ match {
      case Some((p, ps, q, qs)) => (p != i || ps != s) && (q != i || qs != s)
      case None => true
    })
}

object AgentsPattern {
  val empty: AgentsPattern =
    AgentsPattern(Vector.empty, Vector.empty, Nil)

  def addAgent(a: ProteinPattern): State[AgentsPattern, AgentIndex] =
    State(_.addAgent(a))

  def removeAgent(i: AgentIndex): State[AgentsPattern, Unit] =
    State(s => (s.removeAgent(i), ()))

  def requireUnbound(i: AgentIndex, site: Site): State[AgentsPattern, Unit] =
    State(s => (s.requireUnbound(i, site), ()))

  def addLink(i: AgentIndex, si: Site, j: AgentIndex, sj: Site): State[AgentsPattern, LinkId] =
    State(_.link(i, si, j, sj))

  def removeLink(id: LinkId): State[AgentsPattern, Unit] =
    State(s => (s.unlink(id), ()))
}

final case class AgentIndex(value: Int) extends AnyVal
final case class LinkId(value: Int) extends AnyVal
object Unbound

sealed abstract class Action
case class Link(i1: AgentIndex, s1: Site, i2: AgentIndex, s2: Site) extends Action
case class Unlink(id: LinkId) extends Action
case class Modify(i: AgentIndex, rm: ProteinModifications, add: ProteinModifications) extends Action
case class Replace(from: AgentIndex, to: AgentIndex, insert: List[ModifiedProtein]) extends Action
