package protein.demo

import protein.KB
import protein.capability.syntax._
import protein.mechanism.{Binding, Protein, ProteinModifications, Site}

object TestKB extends KB {

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

  def sitesOf(p: Protein): Seq[Site] =
    (bindings.iterator map (_.witness.mentionedSitesOf(p))).reduce(_ union _).toSeq

  def phosphoSites(kinase: Protein, substrate: Protein): Seq[Site] = (kinase, substrate) match {
    case (Protein('C), Protein('B)) => Seq('s)
    case _ => Seq()
  }

  def neighborsOf(p: Protein): Seq[Binding] =
    bindings.iterator.map(r => r.witness.linksAgentTo(p)).flatten.toSeq

  def modsIncreasingKinaseActivity(kinase: Protein): Seq[ProteinModifications] = Seq()

}
