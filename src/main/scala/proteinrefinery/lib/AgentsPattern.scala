package proteinrefinery.lib

import nutcracker.Dom
import nutcracker.syntax.dom._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.util.mapUnion
import proteinrefinery.util.syntax._

import scalaz.Id.Id
import scalaz.{Equal, State}

case class AgentsPattern(
  private val agents: Vector[Option[ProteinPattern]],
  bonds: Vector[Option[(AgentIndex, LocalSiteId, AgentIndex, LocalSiteId)]],
  unbound: List[(AgentIndex, LocalSiteId)]
) {
  lazy val isAdmissible: Boolean = {
    agents forall (_ forall (_.isAdmissible))
    // TODO admissibility of bonds
  }

  def apply(i: AgentIndex): ProteinPattern = agents(i.value).get

  def agentIterator: Iterator[ProteinPattern] = agents.iterator.mapFilter(identity)

  def modify(a: Action): AgentsPattern = a match {
    case Link(i, si, j, sj) => link0(i, si, j, sj)._1
    case Unlink(id) => unlink(id)
    case Modify(i, rmMods, addMods) =>
      ???
    case Replace(from, to, insert) =>
      ???
  }

  def addAgent(a: ProteinPattern): (AgentsPattern, AgentIndex) =
    (copy(agents = agents :+ Some(a)), AgentIndex(agents.size))

  def removeAgent(i: AgentIndex): AgentsPattern = ???

  def updateAgent(i: AgentIndex, u: ProteinPattern.Update): Option[(AgentsPattern, AgentsPattern.Delta)] =
    (this(i): ProteinPattern).update(u) match {
      case Some((pp, δ)) => Some((copy(agents = agents.updated(i.value, Some(pp))), Map(i -> δ)))
      case None => None
    }

  def requireUnbound(i: AgentIndex, s: SiteLabel): AgentsPattern =
    requireUnbound0(i, LocalSiteId(s))

  def requireUnbound0(i: AgentIndex, s: LocalSiteId): AgentsPattern = {
    require(hasAgent(i.value))
    require(isNotBound(i, s))
    AgentsPattern(agents, bonds, (i, s) :: unbound)
  }

  def link(i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel): (AgentsPattern, LinkId) =
    link0(i, LocalSiteId(si), j, LocalSiteId(sj))

  def link0(i: AgentIndex, si: LocalSiteId, j: AgentIndex, sj: LocalSiteId): (AgentsPattern, LinkId) = {
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

  def getBond(id: LinkId): Option[(ProteinPattern, LocalSiteId, ProteinPattern, LocalSiteId)] =
    bonds(id.value).map(reifyBond)

  def getBonds: List[(ProteinPattern, LocalSiteId, ProteinPattern, LocalSiteId)] =
    bonds.iterator.collectToList(_.map(reifyBond))

  def getUnbound: List[(ProteinPattern, LocalSiteId)] =
    unbound map { case (i, s) => (apply(i), s) }

  def unify(that: AgentsPattern): Option[AgentsPattern] = ???
  def partition(that: AgentsPattern): (Option[AgentsPattern], Option[AgentsPattern], Option[AgentsPattern]) = ???

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

  private def reifyBond(b: (AgentIndex, LocalSiteId, AgentIndex, LocalSiteId)): (ProteinPattern, LocalSiteId, ProteinPattern, LocalSiteId) = b match {
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

object AgentsPattern {

  type Update = (AgentIndex, ProteinPattern.Update)
  type Delta = Map[AgentIndex, ProteinPattern.Delta]

  val empty: AgentsPattern =
    AgentsPattern(Vector.empty, Vector.empty, Nil)

  def addAgent(a: ProteinPattern): State[AgentsPattern, AgentIndex] =
    State(_.addAgent(a))

  def removeAgent(i: AgentIndex): State[AgentsPattern, Unit] =
    State(s => (s.removeAgent(i), ()))

  def requireUnbound0(i: AgentIndex, site: LocalSiteId): State[AgentsPattern, Unit] =
    State(s => (s.requireUnbound0(i, site), ()))

  def requireUnbound(i: AgentIndex, site: SiteLabel): State[AgentsPattern, Unit] =
    State(s => (s.requireUnbound(i, site), ()))

  def addLink(i: AgentIndex, si: SiteLabel, j: AgentIndex, sj: SiteLabel): State[AgentsPattern, LinkId] =
    State(_.link(i, si, j, sj))

  def removeLink(id: LinkId): State[AgentsPattern, Unit] =
    State(s => (s.unlink(id), ()))

  implicit def domInstance: Dom.Aux[AgentsPattern, Update, Delta] = new Dom[AgentsPattern] {
    type Update = AgentsPattern.Update
    type Delta = AgentsPattern.Delta

    def update(ap: AgentsPattern, u: Update): Option[(AgentsPattern, Delta)] =
      ap.updateAgent(u._1, u._2)

    def combineDeltas(d1: Delta, d2: Delta): Delta =
      mapUnion[AgentIndex, ProteinPattern.Delta, Id](d1, d2)((δ1, δ2) => Dom[ProteinPattern].combineDeltas(δ1, δ2))

    def assess(ap: AgentsPattern): Dom.Status[Update] =
      if(ap.isAdmissible) Dom.Refined
      else Dom.Failed
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

sealed abstract class Action
case class Link(i1: AgentIndex, s1: LocalSiteId, i2: AgentIndex, s2: LocalSiteId) extends Action
case class Unlink(id: LinkId) extends Action
case class Modify(i: AgentIndex, rm: ProteinModifications, add: ProteinModifications) extends Action
case class Replace(from: AgentIndex, to: AgentIndex, insert: List[ProteinPattern]) extends Action

object Link {
  def apply(i1: AgentIndex, s1: Site.Definite, i2: AgentIndex, s2: Site.Definite): Link =
    Link(i1, LocalSiteId(s1), i2, LocalSiteId(s2))
}