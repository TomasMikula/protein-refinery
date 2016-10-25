package proteinrefinery.lib

import nutcracker.Dom
import nutcracker.syntax.dom._
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.util.{Unification, mapUnion}
import proteinrefinery.util.Unification.Syntax._
import proteinrefinery.util.syntax._

import scalaz.Id.Id
import scalaz.{Equal, State}

case class AgentsPattern(
  agents: Vector[Option[ProteinPattern]],
  bonds: Vector[Option[(AgentIndex, LocalSiteId, AgentIndex, LocalSiteId)]],
  unbound: List[(AgentIndex, LocalSiteId)]
) {
  import AgentsPattern._

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
      case Some((pp, δ)) => Some((copy(agents = agents.updated(i.value, Some(pp))), Delta.update(i, δ)))
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
    i >= 0 && i < agents.size && agents(i).isDefined

  @inline private def getAgent(i: Int): Option[ProteinPattern] =
    if(i >= 0 & i < agents.size) agents(i)
    else None

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

  case class Delta(newAgents: Map[AgentIndex, ProteinPattern], agentDeltas: Map[AgentIndex, ProteinPattern.Delta]) {
    def ifNonEmpty: Option[Delta] =
      if(newAgents.nonEmpty || agentDeltas.nonEmpty) Some(this)
      else None
  }

  object Delta {
    def update(i: AgentIndex, d: ProteinPattern.Delta): Delta =
      Delta(Map(), Map(i -> d))

    def newAgent(i: AgentIndex, a: ProteinPattern): Delta =
      Delta(Map(i -> a), Map())
  }

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

    def combineDeltas(d1: Delta, d2: Delta): Delta = {
      val newAgents = d1.newAgents ++ d2.newAgents
      val deltas = mapUnion[AgentIndex, ProteinPattern.Delta, Id](d1.agentDeltas, d2.agentDeltas)((δ1, δ2) => {
        Dom[ProteinPattern].combineDeltas(δ1, δ2)
      })
      Delta(newAgents, deltas)
    }

    def assess(ap: AgentsPattern): Dom.Status[Update] =
      if(ap.isAdmissible) Dom.Refined
      else Dom.Failed
  }

  implicit def unificationInstance: Unification.Aux[AgentsPattern, Update, Delta] = new Unification[AgentsPattern] {
    type Update = AgentsPattern.Update
    type Delta = AgentsPattern.Delta

    def unify(ap1: AgentsPattern, ap2: AgentsPattern): (Option[Delta], AgentsPattern, Option[Delta]) = {
      val agents = Vector.newBuilder[Option[ProteinPattern]]
      val newAgs1 = Map.newBuilder[AgentIndex, ProteinPattern]
      val newAgs2 = Map.newBuilder[AgentIndex, ProteinPattern]
      val deltas1 = Map.newBuilder[AgentIndex, ProteinPattern.Delta]
      val deltas2 = Map.newBuilder[AgentIndex, ProteinPattern.Delta]

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

    def dom: Dom.Aux[AgentsPattern, Update, Delta] = domInstance
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