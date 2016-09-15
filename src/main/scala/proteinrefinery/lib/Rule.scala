package proteinrefinery.lib

import nutcracker.util.{ContF, FreeK, InjectK}
import nutcracker.{Antichain, PropagationLang, Trigger}
import proteinrefinery.lib.ProteinModifications.LocalSiteId

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scalaz.Show

final case class Rule(lhs: AgentsPattern, actions: List[Action]) {
  lazy val rhs: AgentsPattern = actions.foldLeft(lhs)((p, a) => p.modify(a))
  def apply(lhs: AgentsPattern): AgentsPattern = ???
  def canConsume(ptrn: AgentsPattern): Boolean = ???
  def canProduce(ptrn: AgentsPattern): Boolean = ???

  def mentionedSitesOf(p: Protein): Set[LocalSiteId] = {
    val buf = ArrayBuffer[LocalSiteId]()

    // sites mentioned in agent patterns
    lhs.agentIterator.filter(_.protein == p).foreach(agent => {
      buf ++= agent.mods.finalSiteMods.mods.keysIterator.map(LocalSiteId(_))
      buf ++= agent.mods.nonFinalSiteMods.mods.keysIterator.map(LocalSiteId(_))
    })

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
          buf ++= rm.finalSiteMods.mods.keysIterator.map(LocalSiteId(_))
          buf ++= rm.nonFinalSiteMods.mods.keysIterator.map(LocalSiteId(_))
          buf ++= add.finalSiteMods.mods.keysIterator.map(LocalSiteId(_))
          buf ++= add.nonFinalSiteMods.mods.keysIterator.map(LocalSiteId(_))
        }
      case Replace(_, _, insert) =>
        insert.iterator.filter(_.protein == p).foreach(mp => {
          buf ++= mp.mods.finalSiteMods.mods.keysIterator.map(LocalSiteId(_))
          buf ++= mp.mods.nonFinalSiteMods.mods.keysIterator.map(LocalSiteId(_))
        })
    })

    buf.toSet
  }

  def linksAgentTo(p: Protein): Set[Binding] = {
    val buf = ArrayBuffer[Binding]()
    actions.foldLeft(())({
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
          (q.protein == p) && (addMods.finalSiteMods meet q.mods.finalSiteMods).nonEmpty
        })

      case Replace(from, to, insert) => ???
    }

    actions.exists(enables(_, pat))
  }

  // TODO: should return a list of explanations instead of Boolean
  def enables(that: Rule): Boolean = enables(that.lhs)

  override def toString: String = s"$lhs -> $rhs"
}

object Rule {
  type Ref = Antichain.Ref[Rule]

  def linksAgentToC[F[_[_], _]](ref: Ref)(p: Protein)(implicit inj: InjectK[PropagationLang, F]): ContF[F, Binding.Ref] =
    ContF(f => PropagationLang.domTriggerF(ref)(r => {
      val now = FreeK.sequence_(r.value.linksAgentTo(p).iterator.map(b => PropagationLang.cellF(Antichain(b)).inject[F].flatMap(f)).toList)
      val onChange: (Antichain[Rule], Antichain.Delta[Rule]) => Trigger[FreeK[F, Unit]] = (d, δ) => sys.error("Unreachable code")
      (Some(now), Some(onChange))
    }))

  implicit def showInstance: Show[Rule] = new Show[Rule] {
    override def shows(r: Rule): String = r.toString
  }
}
