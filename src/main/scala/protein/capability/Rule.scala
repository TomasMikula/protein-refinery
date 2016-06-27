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
    lhs.agents.iterator.filter(_.protein == p).foreach(buf ++= _.mods.mods.keys)

    // sites mentioned in existing bonds
    lhs.bonds.foreach({
      case (i, si, j, sj) =>
        if(lhs(i).protein == p) buf += si
        if(lhs(j).protein == p) buf += sj
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

  override def toString: String = s"$lhs -> $rhs"
}

object Rule {
  implicit def showInstance: Show[Rule] = new Show[Rule] {
    override def shows(r: Rule): String = r.toString
  }
}