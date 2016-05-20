package protein.search

import nutcracker.CostLang._
import nutcracker.PropagationLang._
import nutcracker._
import nutcracker.lib.bool._
import nutcracker.util.{FreeK, InjectK}
import protein._
import protein.Cost._
import protein.KBLang._
import protein.capability.BindingPartnerPattern
import protein.mechanism.{Binding, BindingPartner, CompetitiveBinding, Protein, ProteinModifications, Site}
import protein.search.Assoc._

import scalaz.{Applicative, Foldable, IList, Monad, NonEmptyList}

object AssocSearch {

  def search(p: Protein, q: Protein): FreeK[Vocabulary, Promised[mechanism.Assoc]] =
    search0(p, q) >>= { fetch(_) }

  def search0(p: Protein, q: Protein): FreeK[Vocabulary, Assoc] = {
    for {
      /* initialize left end */

      // start with all possible sites, we will be refining the binding site on p later
      ls <- sitesOfF(p) >>>= { variable[Site].oneOf(_:_*).inject[Vocabulary] }

      // promise a binding to the right
      bnd <- promise[Binding].inject[Vocabulary]

      // promise next Elem
      lNext <- promise[LeftConnected].inject[Vocabulary]

      left = LeftEnd(p, ls, bnd, lNext)


      /* initialize right end */

      // start with all possible sites, we will be refining the binding site on q later
      rs <- sitesOfF(q) >>>= { variable[Site].oneOf(_:_*).inject[Vocabulary] }

      // promise previous Elem
      rPrev <- promise[RightConnected].inject[Vocabulary]

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

  private def connect(leftTail: NonEmptyList[RightConnected], rightTail: NonEmptyList[LeftConnected]): FreeK[Vocabulary, Unit] =
    for {
      // find neighbors of left tip
      neighbors <- bindingsOfF(leftTail.head.protein).inject[Vocabulary]

      // branch by trying to connect via each neighbor
      _ <- branchAndExec(neighbors map { n => connectVia(leftTail, rightTail, n) }: _*)
    } yield ()

  private implicit val inj = implicitly[InjectK[protein.CostL, Vocabulary]]
  private def connectVia(leftTail: NonEmptyList[RightConnected], rightTail: NonEmptyList[LeftConnected], b: Binding): FreeK[Vocabulary, Unit] = {
    // in any case, complete the binding in the left tip and
    // set the right-bound site of left tip to the one from the found binding
    complete(leftTail.head.bindingToRight, b) >>>
    set(leftTail.head.toRight, b.leftS).inject[Vocabulary] >> {

      // case 1: the binding connects the tips
      val branch1 =
        if(rightTail.head.protein == b.right) Some(
          // unify the binding site of the right tip
          set(rightTail.head.toLeft, b.rightS) >>>
          // set the pointers
          complete(leftTail.head.right, rightTail.head).inject[Vocabulary] >>
          complete(rightTail.head.left, leftTail.head).inject[Vocabulary]
        ) else None

      // case 2: make the neighbor the new left tip and add distinctness constraints
      val branch2 = for {
        // penalize the additional element of the scaffold
        _ <- costF(complexity(10)).inject[Vocabulary]
        // set the left-bound site of the mid-point according to the found binding
        mls <- variable[Site].oneOf(b.rightS).inject[Vocabulary]
        // initialize right-bound site
        mrs <- sitesOfF(b.right) >>>= { ss => variable[Site].oneOf(ss.filter(_ != b.rightS): _*).inject[Vocabulary] }
        // promise binding to the right
        bnd <- promise[Binding].inject[Vocabulary]
        // promise the neighbors
        mPrev <- promise[RightConnected].inject[Vocabulary]
        mNext <- promise[LeftConnected].inject[Vocabulary]
        // instantiate the mid-point
        mid = MidPoint(b.right, mrs, bnd, mNext, mls, mPrev)
        // connect to the left tip
        _ <- complete(leftTail.head.right, mid).inject[Vocabulary]
        _ <- complete(mPrev, leftTail.head).inject[Vocabulary]
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
      competitiveBinding1(competitor, bnd.leftPattern) map { competingBinding => CompetitiveBinding(bnd.flip, competingBinding) },
      competitiveBinding1(competitor, bnd.rightPattern) map { competingBinding => CompetitiveBinding(bnd, competingBinding) }
    ).flatten

  private def competitiveBinding1(competitor: Protein, bp: BindingPartnerPattern): Cont[Binding] =
    Cont.wrapEffect(for {
      neighbors0 <- bindingsOfF(competitor).inject[Vocabulary]
      neighbors = neighbors0 filter { bnd =>
        bnd.right == bp.p.protein &&
          bnd.rightS == bp.s &&
          (bnd.rightPattern.p.mods combine bp.p.mods).isDefined
      }
      bndRef <- branch(neighbors:_*)
    } yield bndRef.asCont)
}