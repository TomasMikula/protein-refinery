package protein

import protein.capability.syntax._
import protein.lib.{Binding, KB, Protein, Site}

package object demo {

  val TestKB = KB[Prg[Unit]](
    rules = List[Binding](
      /* 00 */ ('A @@ 'b) binds ('B @@ 'v),
      /* 01 */ ('A('z~"p") @@ 'c) binds ('C @@ 'a),
      /* 02 */ ('A @@ 'e) binds ('E @@ 'a),
      /* 03 */ ('C @@ 'a) binds ('E @@ 'c),
      /* 04 */ ('D @@ 'n) binds ('E @@ 'c),
      /* 05 */ ('D @@ 'n) binds ('X @@ 'c),
      /* 06 */ ('B @@ 'v) binds ('Y @@ 'b),
      /* 07 */ ('X @@ 'y) binds ('Y @@ 'x),
      /* 08 */ ('X @@ 'c) binds ('C @@ 'a)
    ).map(_.witness),

    phosphoSites = List[(Protein, Protein, Site)](
      ('C, 'B, 's)
    )
  )

}
