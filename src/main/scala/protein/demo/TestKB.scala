package protein.demo

import nutcracker.util.{FreeK, Lst, _}
import protein._
import protein.capability.syntax._
import protein.mechanism.{Binding, Protein, ProteinModifications, Site}

object TestKB extends KB[FreeK[Vocabulary, ?]] {

  val bindings = List[Binding](
    /* 00 */ ('A @@ 'b) binds ('B @@ 'v),
    /* 01 */ ('A('z~"p") @@ 'c) binds ('C @@ 'a),
    /* 02 */ ('A @@ 'e) binds ('E @@ 'a),
    /* 03 */ ('C @@ 'a) binds ('E @@ 'c),
    /* 04 */ ('D @@ 'n) binds ('E @@ 'c),
    /* 05 */ ('D @@ 'n) binds ('X @@ 'c),
    /* 06 */ ('B @@ 'v) binds ('Y @@ 'b),
    /* 07 */ ('X @@ 'y) binds ('Y @@ 'x),
    /* 08 */ ('X @@ 'c) binds ('C @@ 'a)
  )

  override def phosphoSites(kinase: Protein, substrate: Protein)(f: Site => FreeK[Vocabulary, Unit]): (Lst[FreeK[Vocabulary, Unit]], KB[FreeK[Vocabulary, ?]], Unit) =
    (kinase, substrate) match {
      case (Protein('C), Protein('B)) => (Lst.singleton(f('s)), this, ())
      case _ => (Lst.empty, this, ())
    }

  override def neighborsOf(p: Protein)(f: Binding => FreeK[Vocabulary, Unit]): (Lst[FreeK[Vocabulary, Unit]], KB[FreeK[Vocabulary, ?]], Unit) =
    (Lst.singleton(FreeK.traverse_(bindings.iterator.map(r => r.witness.linksAgentTo(p)).flatten.toSeq)(f)), this, ())

  override def modsIncreasingKinaseActivity(kinase: Protein)(f: ProteinModifications => FreeK[Vocabulary, Unit]): (Lst[FreeK[Vocabulary, Unit]], KB[FreeK[Vocabulary, ?]], Unit) = ???
}
