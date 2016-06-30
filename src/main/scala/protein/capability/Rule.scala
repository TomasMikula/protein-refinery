package protein.capability

import protein.mechanism.{Binding, Protein, Site}

import scala.collection.mutable.ArrayBuffer
import scalaz.{NonEmptyList, Show}
import scalaz.syntax.foldable._

final case class Rule(lhs: AgentsPattern, actions: NonEmptyList[Action]) {
  lazy val rhs: AgentsPattern = actions.foldLeft(lhs)((p, a) => p.modify(a))
  def apply(lhs: Agents): Agents = ???
  def canConsume(ptrn: AgentsPattern): Boolean = ???
  def canProduce(ptrn: AgentsPattern): Boolean = ???

  def mentionedSitesOf(p: Protein): Set[Site] = {
    val buf = ArrayBuffer[Site]()

    // sites mentioned in agent patterns
    lhs.agentIterator.filter(_.protein == p).foreach(buf ++= _.mods.mods.keys)

    // sites mentioned in existing bonds
    lhs.bonds.foreach({
      case Some((i, si, j, sj)) =>
        if(lhs(i).protein == p) buf += si
        if(lhs(j).protein == p) buf += sj
      case None =>
        // do nothing
    })

    // sites mentioned in actions
    actions.map(_ match {
      case Link(i, si, j, sj) =>
        if(lhs(i).protein == p) buf += si
        if(lhs(j).protein == p) buf += sj
      case Unlink(_) => // do nothing
      case Modify(i, rm, add) =>
        if(lhs(i).protein == p) {
          buf ++= rm.mods.keys
          buf ++= add.mods.keys
        }
      case Replace(_, _, insert) =>
        insert.iterator.filter(_.p == p).foreach(mp => buf ++= mp.mods.mods.keys)
    })

    buf.toSet
  }

  def linksAgentTo(p: Protein): Set[Binding] = {
    val buf = ArrayBuffer[Binding]()
    actions.list.foldLeft(())({
      case ((), Link(i, si, j, sj)) =>
        if(lhs(i).protein == p)
          buf += Binding(this, i, j, si, sj)
        if(lhs(j).protein == p)
          buf += Binding(this, j, i, sj, si)
        ()
      case _ => ()
    })
    buf.toSet
  }

  def enables(pat: AgentsPattern): Boolean = {

    def enables(a: Action, pat: AgentsPattern): Boolean = a match {

      case Link(i, si, j, sj) =>
        // does `pat` need this link?
        val pi = lhs(i).protein
        val pj = lhs(j).protein
        pat.getBonds.exists({ case (p, ps, q, qs) =>
          (p.protein == pi && ps == si && q.protein == pj && qs == sj) ||
            (p.protein == pj && ps == sj && q.protein == pi && qs == si)
        })

      case Unlink(linkId) =>
        // does `pat` need one of the participants unbound?
        val (p, ps, q, qs) = lhs.getBond(linkId).get
        pat.getUnbound.exists({ case (pp, s) =>
          (pp.protein == p.protein && s == ps) ||
            (pp.protein == q.protein && s == qs)
        })

      case Modify(i, rmMods, addMods) =>
        // does `pat` need some of the introduced modifications?
        val p = lhs(i).protein
        pat.agentIterator.exists(q => {
          (q.protein == p) && (addMods meet q.mods).mods.nonEmpty
        })

      case Replace(from, to, insert) => ???
    }

    actions.foldLeft(false)((res, action) => res || enables(action, pat))
  }

  // TODO: should return a list of explanations instead of Boolean
  def enables(that: Rule): Boolean = enables(that.lhs)

  override def toString: String = s"$lhs -> $rhs"
}

object Rule {
  implicit def showInstance: Show[Rule] = new Show[Rule] {
    override def shows(r: Rule): String = r.toString
  }
}
