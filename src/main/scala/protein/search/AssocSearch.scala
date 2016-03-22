package protein.search

import nutcracker.CostLang._
import nutcracker.PropagationLang._
import nutcracker._
import nutcracker.lib.bool._
import nutcracker.util.{InjectK, FreeK}
import protein.Cont
import protein._
import protein.Cost._
import protein.capability.{Binding, BindingPartner}
import protein.mechanism.{CompetitiveBinding, Site, ProteinModifications, Protein}
import protein.search.Assoc._

import scalaz.{Applicative, Monad, NonEmptyList, IList, Foldable}


case class AssocSearch(kb: KB) {

  def search(p: Protein, q: Protein): FreeK[Vocabulary, Promised[mechanism.Assoc]] =
    search0(p, q) >>= { fetch(_) }

  def search0(p: Protein, q: Protein): FreeK[Vocabulary, Assoc] = {
    for {
      /* initialize left end */

      // start with all possible sites, we will be refining the binding site on p later
      ls <- variable[Site].oneOf(kb.sitesOf(p):_*).inject[Vocabulary]

      // promise a binding to the right
      bnd <- promiseF[Binding].inject[Vocabulary]

      // promise next Elem
      lNext <- promiseF[LeftConnected].inject[Vocabulary]

      left = LeftEnd(p, ls, bnd, lNext)


      /* initialize right end */

      // start with all possible sites, we will be refining the binding site on q later
      rs <- variable[Site].oneOf(kb.sitesOf(q):_*).inject[Vocabulary]

      // promise previous Elem
      rPrev <- promiseF[RightConnected].inject[Vocabulary]

      right = RightEnd(q, rs, rPrev)


      /* try to connect the ends */
      _ <- connect(NonEmptyList(left), NonEmptyList(right))

    } yield Assoc(left, right)
  }

  def fetch(a: Assoc): FreeK[Vocabulary, Promised[mechanism.Assoc]] =
    promiseC(whenComplete(a))

  private[search] def whenComplete(a: Assoc): Cont[mechanism.Assoc] =
    whenComplete0(a.leftEnd).map(mechanism.Assoc(_))

  private def whenComplete0(elem: RightConnected): Cont[List[Binding]] = for {
    bnd <- elem.bindingToRight.asCont[Vocabulary]
    tail <- elem.right.asCont[Vocabulary]
    bnds <- whenComplete1(tail)
  } yield bnd :: bnds

  private def whenComplete1(elem: LeftConnected): Cont[List[Binding]] =
    elem match {
      case re @ RightEnd(_, _, _) => Monad[Cont].point(Nil)
      case mp @ MidPoint(_, _, _, _, _, _) => whenComplete0(mp)
    }

  private def connect(leftTail: NonEmptyList[RightConnected], rightTail: NonEmptyList[LeftConnected]): FreeK[Vocabulary, Unit] = {
    // find neighbors of left tip
    val neighbors = kb.neighborsOf(leftTail.head.protein)

    // branch by trying to connect via each neighbor
    branchAndExec(neighbors map { n => connectVia(leftTail, rightTail, n) }: _*)
  }

  private implicit val inj = implicitly[InjectK[protein.CostL, Vocabulary]]
  private def connectVia(leftTail: NonEmptyList[RightConnected], rightTail: NonEmptyList[LeftConnected], b: capability.Binding): FreeK[Vocabulary, Unit] = {
    // in any case, complete the binding in the left tip and
    // set the right-bound site of left tip to the one from the found binding
    completeF(leftTail.head.bindingToRight, b) >>>
    set(leftTail.head.toRight, b.left.s).inject[Vocabulary] >> {

      // case 1: the binding connects the tips
      val branch1 =
        if(rightTail.head.protein == b.right.p.p) Some(
          // unify the binding site of the right tip
          set(rightTail.head.toLeft, b.right.s) >>>
          // set the pointers
          completeF(leftTail.head.right, rightTail.head).inject[Vocabulary] >>
          completeF(rightTail.head.left, leftTail.head).inject[Vocabulary]
        ) else None

      // case 2: make the neighbor the new left tip and add distinctness constraints
      val branch2 = for {
        // penalize the additional element of the scaffold
        _ <- costF(complexity(10)).inject[Vocabulary]
        // set the left-bound site of the mid-point according to the found binding
        mls <- variable[Site].oneOf(b.right.s).inject[Vocabulary]
        // initialize right-bound site
        mrs <- variable[Site].oneOf(kb.sitesOf(b.right.p.p).filter(_ != b.right.s): _*).inject[Vocabulary]
        // promise binding to the right
        bnd <- promiseF[Binding].inject[Vocabulary]
        // promise the neighbors
        mPrev <- promiseF[RightConnected].inject[Vocabulary]
        mNext <- promiseF[LeftConnected].inject[Vocabulary]
        // instantiate the mid-point
        mid = MidPoint(b.right.p.p, mrs, bnd, mNext, mls, mPrev)
        // connect to the left tip
        _ <- completeF(leftTail.head.right, mid).inject[Vocabulary]
        _ <- completeF(mPrev, leftTail.head).inject[Vocabulary]
        // add distinctness constraints to make sure the mid-point is different from all other elements in the chain
        _ <- distinctFromAll(mid, leftTail)
        _ <- distinctFromAll(mid, rightTail)
        // recurse
        _ <- connect(mid <:: leftTail, rightTail)
      } yield ()

      branch1.map(b1 => branchAndExec(b1, branch2)).getOrElse(branch2)
    }
  }

  private def distinctFromAll(mid: MidPoint, others: NonEmptyList[_ <: Elem]): FreeK[Vocabulary, Unit] = {
    import algebra.std.set._ // use the implicit GenBool instance for Set
    implicit val app: Applicative[FreeK[Vocabulary, ?]] = FreeK.freeKMonad[Vocabulary] // not sure why scalac cannot find this by itself

    Foldable[IList].sequence_[FreeK[Vocabulary, ?], Unit](others.list filter { _.protein == mid.protein } map {
      case mp @ MidPoint(_, _, _, _, _, _) => for {
        dl <- isDifferent(mid.toLeft, mp.toLeft)
        dr <- isDifferent(mid.toRight, mp.toRight)
        _ <- atLeastOneTrue(dl, dr)
      } yield ()
      case le @ LeftEnd(_, _, _, _) =>
        different(mid.toRight, le.toRight)
      case re @ RightEnd(_, _, _) =>
        different(mid.toLeft, re.toLeft)
    } map { p => p.inject[Vocabulary] })
  }


  def negativeInfluence(p: Protein, a: Assoc): FreeK[Vocabulary, Promised[CompetitiveBinding]] =
    promiseC(negativeInfluence0(p, a.leftEnd))

  private def negativeInfluence0(p: Protein, elem: RightConnected): Cont[CompetitiveBinding] = {
    // Currently, the only way a protein can have a negative influence on association of two proteins,
    // is via a competitive binding on one of the proteins in the association.
    branchC(
      // case 1: competitive binding in binding to the right
      elem.bindingToRight.asCont[Vocabulary] flatMap { bnd => competitiveBinding0(p, bnd) },

      // case 2: competitive binding somewhere in the tail
      elem.right.asCont[Vocabulary] flatMap { negativeInfluence1(p, _) }
    ).flatten
  }

  private def negativeInfluence1(p: Protein, tail: LeftConnected): Cont[CompetitiveBinding] = tail match {
    case RightEnd(_, _, _) => branchC() // empty branching equals fail
    case mp @ MidPoint(_, _, _, _, _, _) => negativeInfluence0(p, mp)
  }

  private def competitiveBinding0(competitor: Protein, bnd: Binding): Cont[CompetitiveBinding] =
    branchC(
      competitiveBinding1(competitor, bnd.left) map { competingBinding => CompetitiveBinding(Binding(bnd.right, competingBinding.right), competingBinding.left) },
      competitiveBinding1(competitor, bnd.right) map { competingBinding => CompetitiveBinding(Binding(bnd.left, competingBinding.right), competingBinding.left) }
    ).flatten

  private def competitiveBinding1(competitor: Protein, bp: BindingPartner): Cont[Binding] = {
    val neighbors = kb.neighborsOf(competitor) filter { bnd =>
      bnd.right.p.p == bp.p.p &&
        bnd.right.s == bp.s &&
        (bnd.right.p.mods combine bp.p.mods).isDefined
    }
    branchC(neighbors:_*)
  }
}