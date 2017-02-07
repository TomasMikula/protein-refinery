package proteinrefinery.lib

import nutcracker.{Discrete, Dom, Promise, Propagation, Trigger}
import nutcracker.Dom.Status
import nutcracker.ops._
import nutcracker.syntax.dom._
import nutcracker.util.{ContU, DeepEqualK, DeepShowK, EqualK, FreeObjectOutput, IsEqual, MonadObjectOutput, ShowK}
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteState.SiteState
import proteinrefinery.util.{OnceTrigger, Unification}
import proteinrefinery.util.Unification.Syntax._

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scalaz.{Lens, Monad, Show, Store}
import scalaz.std.list._
import scalaz.syntax.equal._
import scalaz.syntax.monad._

final case class Rule[Ref[_]] (lhs: AgentsPattern[Ref], actions: List[Action[Ref]])(implicit ev: EqualK[Ref]) {

  lazy val isAdmissible: Boolean = lhs.isAdmissible

  def rhs: AgentsPattern[Ref] = actions.foldLeft(lhs)((p, a) => p.modify(a))
  def apply(lhs: AgentsPattern[Ref]): AgentsPattern[Ref] = ???
  def canConsume(ptrn: AgentsPattern[Ref]): Boolean = ???
  def canProduce(ptrn: AgentsPattern[Ref]): Boolean = ???

  private def setLhs(lhs: AgentsPattern[Ref]): Rule[Ref] =
    Rule(lhs, actions)

  def mentionedSitesOf(p: Protein): Set[LocalSiteId[Ref]] = {
    val buf = ArrayBuffer[LocalSiteId[Ref]]()

    // sites mentioned in agent patterns
    lhs.agentIterator.filter(_.protein == p).foreach(buf ++= _.mentionedSites)

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
      case Modify(i, rm, add, _) =>
        if(lhs(i).protein == p) {
          buf ++= rm.mentionedSites
          buf ++= add.mentionedSites
        }
      case Replace(_, _, insert) =>
        insert.iterator.filter(_.protein == p).foreach(buf ++= _.mentionedSites)
    })

    buf.toSet
  }

  def linksAgentTo(p: Protein): Set[Link[Ref]] = {
    val buf = ArrayBuffer[Link[Ref]]()
    actions.foldLeft(())({
      case ((), l @ Link(i, si, j, sj)) =>
        if(lhs(i).protein == p)
          buf += l
        if(lhs(j).protein == p)
          buf += l.flip
        ()
      case _ => ()
    })
    buf.toSet
  }

  def phosphorylations: List[PhosphoTarget[Ref]] = {
    val buf = actions.foldLeft(List.newBuilder[PhosphoTarget[Ref]])((buf, a) => a match {
      case Modify(i, rm, add, Some(enzyme)) =>
        add.mods.list.foreach(ss =>
          if(ss.state === SiteState("p")) buf += PhosphoTarget(this, enzyme, i, ss.site)
        )
        buf
      case _ => buf
    })
    buf.result()
  }

  def enables(pat: AgentsPattern[Ref]): Boolean = {

    def enables(a: Action[Ref], pat: AgentsPattern[Ref]): Boolean = a match {

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

      case Modify(i, rmMods, addMods, _) =>
        // does `pat` need some of the introduced modifications?
        val p = lhs(i).protein
        pat.agentIterator.exists(q => {
          if(q.protein =/= p) false
          else {
            val addModsMap = addMods.mods.toMap[ISite[Ref], SiteState](_.tuple).mapValues(st => Promise.completed(st))
            val qModsMap   =  q.mods.mods.toMap[ISite[Ref], SiteState](_.tuple).mapValues(st => Promise.completed(st))
            val meet = addModsMap.intersect(qModsMap)((p1, p2) => Promise.meet(p1, p2))
            meet.entries.exists(_._2.nonEmpty)
          }
        })

      case Replace(from, to, insert) => ???
    }

    actions.exists(enables(_, pat))
  }

  // TODO: should return a list of explanations instead of Boolean
  def enables(that: Rule[Ref]): Boolean = enables(that.lhs)

  override def toString: String = show[FreeObjectOutput[String, Ref, ?]].showShallow(ShowK.fromToString)

  def show[F[_]](implicit F: MonadObjectOutput[F, String, Ref]): F[Unit] = {
    val r = lhs.show[F] >> F.write(" -> ") >> rhs.show[F]
    if(lhs.assocs.nonEmpty) F.nest(r) >> lhs.showAssocs[F]
    else                           r
  }
}

object Rule {
  type Update[Var[_]] = AgentsPattern.Update[Var]
  type Delta[Var[_]] = AgentsPattern.Delta[Var]

  /** Lens to access the left-hand side of a rule.
    * Care must be taken that the lhs is updated consistently with actions,
    * e.g. only by refinements.
    */
  def lhs[Var[_]]: Lens[Rule[Var], AgentsPattern[Var]] =
    Lens(r => Store(lhs => r.setLhs(lhs), r.lhs))

  type Ref[Var[_]] = Var[Discrete[Rule[Var]]]

  implicit val deepEqualKInstance: DeepEqualK[Rule, Rule] = new DeepEqualK[Rule, Rule] {
    def equal[Ptr1[_], Ptr2[_]](r1: Rule[Ptr1], r2: Rule[Ptr2]): IsEqual[Ptr1, Ptr2] =
      IsEqual(r1.lhs, r2.lhs) && IsEqual(r1.actions, r2.actions)
  }

  implicit val deepShowKInstance: DeepShowK[Rule] = new DeepShowK[Rule] {
    def show[Ptr[_], M[_]](a: Rule[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      a.show[M]
  }

  implicit def domInstance[Var[_]](implicit ev: EqualK[Var]): Dom.Aux[Rule[Var], Update[Var], Delta[Var]] = new Dom[Rule[Var]] {
    type Update = Rule.Update[Var]
    type Delta = Rule.Delta[Var]

    def update(r: Rule[Var], u: Update): Option[(Rule[Var], Delta)] = {
      val Rule(lhs, actions) = r
      lhs.update(u) match {
        case Some((lhs, δ)) => Some((Rule(lhs, actions), δ))
        case None => None
      }
    }

    def combineDeltas(d1: Delta, d2: Delta): Delta =
      Dom[AgentsPattern[Var]].combineDeltas(d1, d2)

    def assess(r: Rule[Var]): Status[Update] =
      Dom[AgentsPattern[Var]].assess(r.lhs)
  }

  implicit def unificationInstance[Var[_]](implicit ev: EqualK[Var]): Unification.Aux[Rule[Var], Update[Var], Delta[Var]] = new Unification[Rule[Var]] {
    type Update = Rule.Update[Var]
    type Delta = Rule.Delta[Var]

    def unify(r1: Rule[Var], r2: Rule[Var]): (Option[Delta], Rule[Var], Option[Delta]) = {
      val Rule(lhs1, actions1) = r1
      val Rule(lhs2, actions2) = r2
      val (d1, lhs, d2) = lhs1 unify lhs2
      val actions = (r1.actions ++ r2.actions).distinct // XXX change to actions should be part of Delta
      (d1, Rule(lhs, actions), d2)
    }

    def dom: Dom.Aux[Rule[Var], Update, Delta] = domInstance
  }

  implicit def showInstance[Var[_]]: Show[Rule[Var]] = new Show[Rule[Var]] {
    override def shows(r: Rule[Var]): String = r.toString
  }

  trait Ops[M[_], Var[_]] {
    implicit val Propagation: Propagation[M, Var]

    def Nuggets: Nuggets[M, Var]
    def AgentsPatternOps: AgentsPattern.Ops[M, Var]

    import Propagation._

    def linksAgentToC(ref: Rule.Ref[Var])(p: Protein)(implicit M: Monad[M]): ContU[M, Binding[Var]] =
      ContU(f => observe(ref).by(r => {
        import scalaz.syntax.traverse._
        val now = r.value.linksAgentTo(p).iterator.map(l => f(Binding(ref, l))).toList.sequence_
        val onChange: (Discrete[Rule[Var]], Discrete.Delta[Rule[Var]]) => Trigger[M[Unit]] = (d, δ) => sys.error("Unreachable code")
        (Some(now), Some(onChange))
      }))

    def phosphorylationsC(ref: Rule.Ref[Var])(implicit M: Monad[M]): ContU[M, PhosphoTarget.Ref[Var]] =
      ContU(f => observe(ref).by(r => {
        import scalaz.syntax.traverse._
        val now = r.value.phosphorylations.iterator.map(p => cell(Discrete(p)).flatMap(f)).toList.sequence_
        val onChange: (Discrete[Rule[Var]], Discrete.Delta[Rule[Var]]) => Trigger[M[Unit]] = (d, δ) => sys.error("Unreachable code")
        (Some(now), Some(onChange))
      }))

    def enablersOfC(ref: Rule.Ref[Var]): ContU[M, Rule.Ref[Var]] = for {
      r <- ref.asCont[M]
      q <- Nuggets.rulesC(q => if(q.enables(r)) OnceTrigger.Fire(()) else OnceTrigger.Discard())
    } yield q

    def associationsOfC(ref: Rule.Ref[Var])(implicit M: Monad[M]): ContU[M, Assoc.Ref[Var]] =
      ref.asCont[M] flatMap { r => AgentsPatternOps.forEachAssoc(r.lhs) }
  }
}
