package protein

import scala.language.higherKinds
import nutcracker.util.{Lst, Step, WriterState}
import protein.KBLang._
import protein.capability.Rule
import protein.db.{DB, Table, TableId}
import protein.mechanism.{Binding, Protein, ProteinModifications, Site}

/**
  * Knowledge base.
  *
  * @tparam K type of callbacks that are executed for query results.
  */
final case class KB[K] private(private val db: DB[K]) extends AnyVal {
  import KB._

  def addRule(r: Rule): (Lst[K], KB[K], Unit) =
    db.insert(Tables.Rules, r) match { case (db, ks) => (ks, KB(db), ()) }

  def addPhosphoSite(kinase: Protein, substrate: Protein, site: Site): (Lst[K], KB[K], Unit) =
    db.insert(Tables.PhosphoSites, (kinase, substrate, site)) match { case (db, ks) => (ks, KB(db), ()) }

  def neighborsOf(p: Protein)(f: Binding => K): (Lst[K], KB[K], Unit) =
    db.query(Tables.Rules)(rule => (rule.linksAgentTo(p), f) mapRev_::: Lst.empty) match {
      case (db, ks) => (ks, KB(db), ())
    }

  def phosphoSites(kinase: Protein, substrate: Protein)(f: Site => K): (Lst[K], KB[K], Unit) =
    db.query(Tables.PhosphoSites)(kss => {
      val (kin, sub, s)  = kss
      if(kin == kinase && sub == substrate) Lst.singleton(f(s))
      else Lst.empty[K]
    }) match { case (db, ks) => (ks, KB(db), ()) }

  def modsIncreasingKinaseActivity(kinase: Protein)(f: ProteinModifications => K): (Lst[K], KB[K], Unit) = ???

}

object KB {
  private object Tables {
    object Rules extends TableId[Rule] { self: Singleton => }
    object PhosphoSites extends TableId[(Protein, Protein, Site)] { self: Singleton => }
  }

  def apply[K](
    rules: List[Rule] = Nil,
    phosphoSites: List[(Protein, Protein, Site)] = Nil
  ): KB[K] = {
    val db = DB.empty[K]
      .setTable(Tables.Rules, Table[K, Rule](rules))
      .setTable(Tables.PhosphoSites, Table[K, (Protein, Protein, Site)](phosphoSites))
    KB(db)
  }

  implicit def interpreter: Step[KBLang, KB] = new Step[KBLang, KB] {
    def apply[K[_], A](op: KBLang[K, A]): WriterState[Lst[K[Unit]], KB[K[Unit]], A] = WriterState(kb => op match {
      case AddRule(r) => kb.addRule(r)
      case AddPhosphoSite(kin, sub, s) => kb.addPhosphoSite(kin, sub, s)
      case BindingsOf(p, f) => kb.neighborsOf(p)(f)
      case PhosphoSites(k, s, f) => kb.phosphoSites(k, s)(f)
    })
  }
}