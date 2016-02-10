package protein.search

import nutcracker.BranchLang._
import nutcracker.CostLang._
import nutcracker.PromiseLang._
import nutcracker.PropagationLang._
import nutcracker._
import nutcracker.lib.bool._
import nutcracker.util.free.{InjectK, FreeK}
import protein._
import protein.Cost._
import protein.capability.{Binding, BindingPartner}
import protein.mechanism.{CompetitiveBinding, Site, ProteinModifications, Protein}
import protein.search.Assoc._

import scalaz.Id._
import scalaz.{Foldable, Applicative, Apply, StreamT, NonEmptyList}


case class AssocSearch(kb: KB) {

  def search(p: Protein, q: Protein): FreeK[Vocabulary, Promised[mechanism.Assoc]] =
    search0(p, q) >>= { fetch(_) }

  def search0(p: Protein, q: Protein): FreeK[Vocabulary, Assoc] = {
    for {
      /* initialize left end */

      // protein is given
      lp <- variable[Protein].oneOf(p).inject[Vocabulary]

      // start with no protein modification restrictions
      lmods <- variable[ProteinModifications].init(Option(ProteinModifications.noModifications)).inject[Vocabulary]

      // start with all possible sites, we will be refining the binding site on p later
      ls <- variable[Site].oneOf(kb.sitesOf(p):_*).inject[Vocabulary]

      // promise next Elem
      lNext <- promiseF[LeftConnected].inject[Vocabulary]

      left = LeftEnd(lp, lmods, ls, lNext)


      /* initialize right end */

      // protein is given
      rp <- variable[Protein].oneOf(q).inject[Vocabulary]

      // start with no protein modification restrictions
      rmods <- variable[ProteinModifications].init(Option(ProteinModifications.noModifications)).inject[Vocabulary]

      // start with all possible sites, we will be refining the binding site on q later
      rs <- variable[Site].oneOf(kb.sitesOf(q):_*).inject[Vocabulary]

      // promise previous Elem
      rPrev <- promiseF[RightConnected].inject[Vocabulary]

      right = RightEnd(rp, rmods, rs, rPrev)


      /* try to connect the ends */
      _ <- connect(NonEmptyList(left), NonEmptyList(right))

    } yield Assoc(left, right)
  }

  def fetch(a: Assoc): FreeK[Vocabulary, Promised[mechanism.Assoc]] =
    promiseC(whenComplete(a))

  private[search] def whenComplete(a: Assoc): Cont[mechanism.Assoc] = for {
    le <- whenComplete(a.leftEnd)
    tail <- a.leftEnd.right.asCont[Vocabulary]
    msre <- whenComplete1(tail)
    (ms, re) = msre
  } yield mechanism.Assoc(le, ms, re)

  private def whenComplete(le: LeftEnd): Cont[mechanism.Assoc.LeftEnd] =
    Apply[Cont].apply3(le.protein, le.condition, le.toRight)(mechanism.Assoc.LeftEnd(_, _, _))

  private def whenComplete(re: RightEnd): Cont[mechanism.Assoc.RightEnd] =
    Apply[Cont].apply3(re.protein, re.condition, re.toLeft)(mechanism.Assoc.RightEnd(_, _, _))

  private def whenComplete(mp: MidPoint): Cont[mechanism.Assoc.MidPoint] = {
    Apply[Cont].apply4(mp.protein, mp.condition, mp.toRight, mp.toLeft)(mechanism.Assoc.MidPoint(_, _, _, _))
  }

  private def whenComplete1(elem: LeftConnected): Cont[(List[mechanism.Assoc.MidPoint], mechanism.Assoc.RightEnd)] =
    elem match {
      case re @ RightEnd(_, _, _, _) => whenComplete(re) map { (Nil, _) }
      case mp @ MidPoint(_, _, _, _, _, _) => for {
        m <- whenComplete(mp)
        lc <- mp.right.asCont[Vocabulary]
        msre <- whenComplete1(lc)
        (ms, re) = msre
      } yield (m::ms, re)
    }

  private def withBindingToLeft(elem: LeftConnected): Cont[BindingPartner] =
    Apply[Cont].apply3(elem.protein, elem.condition, elem.toLeft)(BindingPartner(_, _, _))

  private def withBindingToRight(elem: RightConnected): Cont[BindingPartner] =
    Apply[Cont].apply3(elem.protein, elem.condition, elem.toRight)(BindingPartner(_, _, _))

  private def connect(leftTail: NonEmptyList[RightConnected], rightTail: NonEmptyList[LeftConnected]): FreeK[Vocabulary, Unit] = {
    whenResolvedF(leftTail.head.protein) { p =>
      // find neighbors of left tip
      val neighbors = kb.neighborsOf(p)

      // branch by trying to connect via each neighbor
      addBranchingF[StreamT[Id, ?], Vocabulary](
        StreamT.fromIterable(neighbors) map { n => connectVia(leftTail, rightTail, n) })(
        implicitly[InjectK[protein.SearchLang.BranchL, Vocabulary]])
    }
  }

  private implicit val inj = implicitly[InjectK[protein.SearchLang.CostL, Vocabulary]]
  private def connectVia(leftTail: NonEmptyList[RightConnected], rightTail: NonEmptyList[LeftConnected], b: capability.Binding): FreeK[Vocabulary, Unit] = {
    // in any case, set the right-bound site of left tip to the one from the found binding
    // and apply the found condition under which this binding can occur
    set(leftTail.head.toRight, b.left.s) >>
    set(leftTail.head.condition, b.left.p.mods) >>>
    branch(
      // case 1: unify neighbor with the right tip
      //  - unify the protein of the right tip
      set(rightTail.head.protein, b.right.p.p) >>
      //  - unify the binding site of the right tip
      set(rightTail.head.toLeft, b.right.s) >>
      //  - apply the state conditions to the right tip
      set(rightTail.head.condition, b.right.p.mods) >>>
      //  - complete the promises
      completeF(leftTail.head.right, rightTail.head).inject[Vocabulary] >>
      completeF(rightTail.head.left, leftTail.head).inject[Vocabulary],

      // case 2: make the neighbor the new left tip and add distinctness constraints
      for {
        // penalize the additional element of the scaffold
        _ <- costF(complexity(10)).inject[Vocabulary]
        // set the protein of the mid-point according to the found binding
        mp <- variable[Protein].oneOf(b.right.p.p).inject[Vocabulary]
        // set the left-bound site of the mid-point according to the found binding
        mls <- variable[Site].oneOf(b.right.s).inject[Vocabulary]
        // initialize right-bound site
        mrs <- variable[Site].oneOf(kb.sitesOf(b.right.p.p).filter(_ != b.right.s):_*).inject[Vocabulary]
        // apply modification constraints to the mid-point according to the found binding
        mmods <- variable[ProteinModifications].init(Option(b.right.p.mods)).inject[Vocabulary]
        // promise the neighbors
        mPrev <- promiseF[RightConnected].inject[Vocabulary]
        mNext <- promiseF[LeftConnected].inject[Vocabulary]
        // instantiate the mid-point
        mid = MidPoint(mp, mmods, mrs, mNext, mls, mPrev)
        // connect to the left tip
        _ <- completeF(leftTail.head.right, mid).inject[Vocabulary]
        _ <- completeF(mPrev, leftTail.head).inject[Vocabulary]
        // add distinctness constraints to make sure the mid-point is different from all other elements in the chain
        _ <- distinctFromAll(mid, leftTail)
        _ <- distinctFromAll(mid, rightTail)
        // recurse
        _ <- connect(mid <:: leftTail, rightTail)
      } yield ()
    )
  }

  private def distinctFromAll(mid: MidPoint, others: NonEmptyList[_ <: Elem]): FreeK[Vocabulary, Unit] = {
    import algebra.std.set._ // use the implicit GenBool instance for Set
    implicit val app: Applicative[FreeK[Vocabulary, ?]] = FreeK.freeKMonad[Vocabulary] // not sure why scalac cannot find this by itself

    Foldable[NonEmptyList].sequence_[FreeK[Vocabulary, ?], Unit](others map {
      case MidPoint(p, c, rs, r, ls, l) => for {
        dp <- isDifferent(mid.protein, p)
        dl <- isDifferent(mid.toLeft, ls)
        dr <- isDifferent(mid.toRight, rs)
        _ <- atLeastOneTrue(dp, dl, dr)
      } yield ()
      case LeftEnd(p, c, rs, r) => for {
        dp <- isDifferent(mid.protein, p)
        dr <- isDifferent(mid.toRight, rs)
        _ <- atLeastOneTrue(dp, dr)
      } yield ()
      case RightEnd(p, c, ls, l) => for {
        dp <- isDifferent(mid.protein, p)
        dl <- isDifferent(mid.toLeft, ls)
        _ <- atLeastOneTrue(dp, dl)
      } yield ()
    } map { p => p.inject[Vocabulary] })
  }


  def negativeInfluence(p: Protein, a: Assoc): FreeK[Vocabulary, Promised[CompetitiveBinding]] = for {
    pr <- promiseF[CompetitiveBinding].inject[Vocabulary]
    _ <- negativeInfluence0(p, a)(cb => completeF(pr, cb).inject[Vocabulary])
  } yield pr

  def negativeInfluence0(p: Protein, a: Assoc)(callback: CompetitiveBinding => FreeK[Vocabulary, Unit]): FreeK[Vocabulary, Unit] = {
    // Currently, the only way a protein can have a negative influence on association of two proteins,
    // is via a competitive binding on one of the proteins in the association.
    branch(
      // case 1: competitive binding on the left end
      competitiveBinding(p, a.leftEnd.protein, a.leftEnd.condition, a.leftEnd.toRight, competingBinding => {
        onCompleteF(a.leftEnd.right){ lc =>
          withBindingToLeft(lc){ bp =>
            callback(CompetitiveBinding(Binding(bp, competingBinding.right), competingBinding.left))
          }
        }
      }),

      // case 2: competitive binding somewhere in the tail
      onCompleteF(a.leftEnd.right){ negativeInfluence(p, _, callback) }
    )
  }

  private def negativeInfluence(p: Protein, tail: LeftConnected, callback: CompetitiveBinding => FreeK[Vocabulary, Unit]): FreeK[Vocabulary, Unit] = tail match {
    case RightEnd(p2, c, ls, l) => competitiveBinding(p, p2, c, ls, competingBinding => {
      onCompleteF(l){ rc =>
        withBindingToRight(rc){ bp =>
          callback(CompetitiveBinding(Binding(bp, competingBinding.right), competingBinding.left))
        }
      }
    })
    case MidPoint(p2, c, rs, r, ls, l) => branch(
      // competitive binding on the site bound to the left
      competitiveBinding(p, p2, c, ls, competingBinding => {
        onCompleteF(l){ rc =>
          withBindingToRight(rc){ bp =>
            callback(CompetitiveBinding(Binding(bp, competingBinding.right), competingBinding.left))
          }
        }
      }),

      // competitive binding on the site bound to the right
      competitiveBinding(p, p2, c, rs, competingBinding => {
        onCompleteF(r){ lc =>
          withBindingToLeft(lc){ bp =>
            callback(CompetitiveBinding(Binding(bp, competingBinding.right), competingBinding.left))
          }
        }
      }),

      // competitive binding somewhere in the tail
      onCompleteF(r){ negativeInfluence(p, _, callback) }
    )
  }

  private def competitiveBinding(
    p: Protein,
    pRef: DomRef[Protein, Set[Protein]],
    cRef: DomRef[ProteinModifications, ProteinModificationsLattice],
    sRef: DomRef[Site, Set[Site]],
    callback: Binding => FreeK[Vocabulary, Unit]
  ): FreeK[Vocabulary, Unit] = branch(
    (kb.neighborsOf(p) map { bnd =>
      set(pRef, bnd.right.p.p) >>
        set(cRef, bnd.right.p.mods) >>
        set(sRef, bnd.right.s) >>>
        callback(bnd)
    }):_*
  )
}