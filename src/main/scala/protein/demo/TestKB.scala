package protein.demo

import protein.KB
import protein.capability.Binding
import protein.capability.syntax._
import protein.mechanism.{Site, Protein}

object TestKB extends KB {

  private val bindings = List[Binding](
    ('A @@ 'b) binds ('B @@ 'v),
    ('A('z~"p") @@ 'c) binds ('C @@ 'a),
    ('A @@ 'e) binds ('E @@ 'a),
    ('C @@ 'a) binds ('E @@ 'c),
    ('D @@ 'n) binds ('E @@ 'c),
    ('D @@ 'n) binds ('X @@ 'c),
    ('B @@ 'v) binds ('Y @@ 'b),
    ('X @@ 'y) binds ('Y @@ 'x),
    ('X @@ 'c) binds ('C @@ 'a)
  )

  def sitesOf(p: Protein): Seq[Site] = (
    (bindings.iterator filter { _.left.p.p == p } map { _.left.s }).toSet ++
    (bindings.iterator filter { _.right.p.p == p } map { _.right.s })
  ).toSeq

  def phosphoSites(kinase: Protein, substrate: Protein): Seq[Site] = (kinase, substrate) match {
    case (Protein('C), Protein('B)) => Seq('s)
    case _ => Seq()
  }

  def neighborsOf(p: Protein): Seq[Binding] =
    (bindings filter { _.left.p.p == p  }) ++
    (bindings filter { _.right.p.p == p } map { _.flip })
}
