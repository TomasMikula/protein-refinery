package proteinrefinery.lib

import nutcracker.Dom
import nutcracker.Dom.Status
import nutcracker.syntax.dom._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.util.mapUnion
import proteinrefinery.util.syntax._

import scalaz.Id.Id
import scalaz.State

sealed trait AgentsPattern

object AgentsPattern {

  type Update = (AgentIndex, ProteinPattern.Update)
  type Delta = Map[AgentIndex, ProteinPattern.Delta]

  implicit def domInstance: Dom.Aux[AgentsPattern, Update, Delta] = new Dom[AgentsPattern] {
    type Update = AgentsPattern.Update
    type Delta = AgentsPattern.Delta

    def update(p: AgentsPattern, u: Update): Option[(AgentsPattern, Delta)] = p match {
      case aap @ AdmissibleAgentsPattern(_, _, _) => aap.updateAgent(u._1, u._2)
      case InvalidAgentsPattern => None
    }

    def combineDeltas(d1: Delta, d2: Delta): Delta =
      mapUnion[AgentIndex, ProteinPattern.Delta, Id](d1, d2)((δ1, δ2) => Dom[ProteinPattern].combineDeltas(δ1, δ2))

    def assess(p: AgentsPattern): Status[Update] = p match {
      case AdmissibleAgentsPattern(_, _, _) => Dom.Refined
      case InvalidAgentsPattern => Dom.Failed
    }
  }

}

case object InvalidAgentsPattern extends AgentsPattern

case class AdmissibleAgentsPattern(
  private val agents: Vector[Option[AdmissibleProteinPattern]],
  bonds: Vector[Option[(AgentIndex, LocalSiteId, AgentIndex, LocalSiteId)]],
  unbound: List[(AgentIndex, LocalSiteId)]
) extends AgentsPattern {

  def apply(i: AgentIndex): AdmissibleProteinPattern = agents(i.value).get

  def agentIterator: Iterator[AdmissibleProteinPattern] = agents.iterator.mapFilter(identity)

  def modify(a: Action): AdmissibleAgentsPattern = a match {
    case Link(i, si, j, sj) => link0(i, si, j, sj)._1
    case Unlink(id) => unlink(id)
    case Modify(i, rmMods, addMods) =>
      ???
    case Replace(from, to, insert) =>
      ???
  }

  def addAgent(a: AdmissibleProteinPattern): (AdmissibleAgentsPattern, AgentIndex) =
    (copy(agents = agents :+ Some(a)), AgentIndex(agents.size))

  def removeAgent(i: AgentIndex): AdmissibleAgentsPattern = ???

  def updateAgent(i: AgentIndex, u: ProteinPattern.Update): Option[(AgentsPattern, AgentsPattern.Delta)] =
    (this(i): ProteinPattern).update(u) match {
      case Some((pp, δ)) => pp match {
        case app @ AdmissibleProteinPattern(_, _) => Some((copy(agents = agents.updated(i.value, Some(app))), Map(i -> δ)))
        case InvalidProteinPattern => Some((InvalidAgentsPattern, Map(i -> δ)))
      }
      case None => None
    }

  def requireUnbound(i: AgentIndex, s: SiteLabel): AdmissibleAgentsPattern =
    requireUnbound0(i, LocalSiteId(s))

  def requireUnbound0(i: AgentIndex, s: LocalSiteId): AdmissibleAgentsPattern = {
    require(hasAgent(i.value))
    require(isNotBound(i, s))
    AdmissibleAgentsPattern(agents, bonds, (i, s) :: unbound)
  }

  def link(i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel): (AdmissibleAgentsPattern, LinkId) =
    link0(i, LocalSiteId(si), j, LocalSiteId(sj))

  def link0(i: AgentIndex, si: LocalSiteId, j: AgentIndex, sj: LocalSiteId): (AdmissibleAgentsPattern, LinkId) = {
    require(hasAgent(i.value))
    require(hasAgent(j.value))
    require(isUnbound(i, si))
    require(isUnbound(j, sj))
    (AdmissibleAgentsPattern(agents, bonds :+ Some((i, si, j, sj)), unbound.filter(u => u != ((i, si)) && u != ((j, sj)))), LinkId(bonds.size))
  }

  def unlink(id: LinkId): AdmissibleAgentsPattern = {
    require(hasBond(id.value))
    val Some((i, si, j, sj)) = bonds(id.value)
    AdmissibleAgentsPattern(agents, bonds.updated(id.value, None), (i, si) :: (j, sj) :: unbound)
  }

  def getBond(id: LinkId): Option[(AdmissibleProteinPattern, LocalSiteId, AdmissibleProteinPattern, LocalSiteId)] =
    bonds(id.value).map(reifyBond)

  def getBonds: List[(AdmissibleProteinPattern, LocalSiteId, AdmissibleProteinPattern, LocalSiteId)] =
    bonds.iterator.collectToList(_.map(reifyBond))

  def getUnbound: List[(AdmissibleProteinPattern, LocalSiteId)] =
    unbound map { case (i, s) => (apply(i), s) }

  def unify(that: AdmissibleAgentsPattern): Option[AdmissibleAgentsPattern] = ???
  def partition(that: AdmissibleAgentsPattern): (Option[AdmissibleAgentsPattern], Option[AdmissibleAgentsPattern], Option[AdmissibleAgentsPattern]) = ???

  override def toString: String = {
    val bondsByAgent = bonds.iterator.zipWithIndex.mapFilter({ case (l, i) => l.map((_, i)) }).flatMap[(AgentIndex, (LocalSiteId, Either[Unbound.type , LinkId]))]{
      case ((pi, ps, qi, qs), linkIdx) =>
        Iterator((pi, (ps, Right(LinkId(linkIdx)))), (qi, (qs, Right(LinkId(linkIdx)))))
    }
    val nonBondsByAgent = unbound.iterator.map[(AgentIndex, (LocalSiteId, Either[Unbound.type , LinkId]))]{
      case (i, s) => (i, (s, Left(Unbound)))
    }
    val linksByAgent = (bondsByAgent ++ nonBondsByAgent).toMultiMap[AgentIndex, (LocalSiteId, Either[Unbound.type , LinkId])]

    agents.iterator.zipWithIndex.mapFilter({ case (pp, i) => pp.map(pp =>
      pp.toString(linksByAgent.getOrElse(AgentIndex(i), Nil).toMap)
    )}).mkString(", ")
  }

  private def reifyBond(b: (AgentIndex, LocalSiteId, AgentIndex, LocalSiteId)): (AdmissibleProteinPattern, LocalSiteId, AdmissibleProteinPattern, LocalSiteId) = b match {
    case (i, si, j, sj) => (apply(i), si, apply(j), sj)
  }

  @inline private def hasAgent(i: Int): Boolean =
    i >= 0 && i < agents.size && agents(i) != null

  @inline private def hasBond(i: Int): Boolean =
    i >= 0 && i < bonds.size && bonds(i).isDefined

  @inline private def isUnbound(i: AgentIndex, s: LocalSiteId): Boolean =
    unbound.contains((i, s))

  @inline private def isNotBound(i: AgentIndex, s: LocalSiteId): Boolean =
    bonds.forall(_ match {
      case Some((p, ps, q, qs)) => (p != i || ps != s) && (q != i || qs != s)
      case None => true
    })
}

object AdmissibleAgentsPattern {
  val empty: AdmissibleAgentsPattern =
    AdmissibleAgentsPattern(Vector.empty, Vector.empty, Nil)

  def addAgent(a: AdmissibleProteinPattern): State[AdmissibleAgentsPattern, AgentIndex] =
    State(_.addAgent(a))

  def removeAgent(i: AgentIndex): State[AdmissibleAgentsPattern, Unit] =
    State(s => (s.removeAgent(i), ()))

  def requireUnbound0(i: AgentIndex, site: LocalSiteId): State[AdmissibleAgentsPattern, Unit] =
    State(s => (s.requireUnbound0(i, site), ()))

  def requireUnbound(i: AgentIndex, site: SiteLabel): State[AdmissibleAgentsPattern, Unit] =
    State(s => (s.requireUnbound(i, site), ()))

  def addLink(i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel): State[AdmissibleAgentsPattern, LinkId] =
    State(_.link(i, si, j, sj))

  def removeLink(id: LinkId): State[AdmissibleAgentsPattern, Unit] =
    State(s => (s.unlink(id), ()))
}

final case class AgentIndex(value: Int) extends AnyVal
final case class LinkId(value: Int) extends AnyVal
object Unbound

sealed abstract class Action
case class Link(i1: AgentIndex, s1: LocalSiteId, i2: AgentIndex, s2: LocalSiteId) extends Action
case class Unlink(id: LinkId) extends Action
case class Modify(i: AgentIndex, rm: AdmissibleProteinModifications, add: AdmissibleProteinModifications) extends Action
case class Replace(from: AgentIndex, to: AgentIndex, insert: List[AdmissibleProteinPattern]) extends Action

object Link {
  def apply(i1: AgentIndex, s1: Site.Definite, i2: AgentIndex, s2: Site.Definite): Link =
    Link(i1, LocalSiteId(s1), i2, LocalSiteId(s2))
}