package protein.capability

import protein.mechanism.{ModifiedProtein, ProteinModifications, Site}

import scalaz.State

case class AgentsPattern(agents: Vector[ProteinPattern], bonds: Vector[(AgentIndex, Site, AgentIndex, Site)]) {

  def apply(i: AgentIndex): ProteinPattern = agents(i.value)

  def modify(a: Action): AgentsPattern = a match {
    case Link(i, si, j, sj) => addLink(i, si, j, sj)._1
    case Unlink(id) => removeLink(id)
    case Modify(i, rmMods, addMods) =>
      ???
    case Replace(from, to, insert) =>
      ???
  }

  def addAgent(a: ProteinPattern): (AgentsPattern, AgentIndex) =
    (copy(agents = agents :+ a), AgentIndex(agents.size))

  def removeAgent(i: AgentIndex): AgentsPattern = ???

  def addLink(i: AgentIndex, si: Site, j: AgentIndex, sj: Site): (AgentsPattern, LinkId) = {
    require(hasAgent(i.value))
    require(hasAgent(j.value))
    (AgentsPattern(agents, bonds :+ ((i, si, j, sj))), LinkId(bonds.size))
  }

  def removeLink(id: LinkId): AgentsPattern = {
    require(hasBond(id.value))
    AgentsPattern(agents, bonds.updated(id.value, null))
  }

  def unify(that: AgentsPattern): Option[AgentsPattern] = ???
  def partition(that: AgentsPattern): (Option[AgentsPattern], Option[AgentsPattern], Option[AgentsPattern]) = ???

  @inline private def hasAgent(i: Int): Boolean =
    i >= 0 && i < agents.size && agents(i) != null

  @inline private def hasBond(i: Int): Boolean =
    i >= 0 && i < bonds.size && bonds(i) != null
}

object AgentsPattern {
  val empty: AgentsPattern =
    AgentsPattern(Vector.empty, Vector.empty)

  def addAgent(a: ProteinPattern): State[AgentsPattern, AgentIndex] =
    State(_.addAgent(a))

  def removeAgent(i: AgentIndex): State[AgentsPattern, Unit] =
    State(s => (s.removeAgent(i), ()))

  def addLink(i: AgentIndex, si: Site, j: AgentIndex, sj: Site): State[AgentsPattern, LinkId] =
    State(_.addLink(i, si, j, sj))

  def removeLink(id: LinkId): State[AgentsPattern, Unit] =
    State(s => (s.removeLink(id), ()))
}

final case class AgentIndex(value: Int) extends AnyVal
final case class LinkId(value: Int) extends AnyVal

sealed abstract class Action
case class Link(i1: AgentIndex, s1: Site, i2: AgentIndex, s2: Site) extends Action
case class Unlink(id: LinkId) extends Action
case class Modify(i: AgentIndex, rm: ProteinModifications, add: ProteinModifications) extends Action
case class Replace(from: AgentIndex, to: AgentIndex, insert: List[ModifiedProtein]) extends Action