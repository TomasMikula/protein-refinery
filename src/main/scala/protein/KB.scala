package protein

import scala.language.higherKinds
import nutcracker.util.{Lst, Step, WriterState}
import protein.KBLang._
import protein.capability.{ProteinPattern, Rule}
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

  def addKinase(kinase: ProteinPattern): (Lst[K], KB[K], Unit) =
    db.insert(Tables.Kinases, kinase) match { case (db, ks) => (ks, KB(db), ()) }

  def rules(f: Rule => Lst[K]): (Lst[K], KB[K], Unit) =
    db.query(Tables.Rules)(f) match { case (db, ks) => (ks, KB(db), ()) }

  def phosphoTargets(f: (Protein, Protein, Site) => K): (Lst[K], KB[K], Unit) =
    db.query(Tables.PhosphoSites)(kss => {
      val (kin, sub, s)  = kss
      Lst.singleton(f(kin, sub, s))
    }) match { case (db, ks) => (ks, KB(db), ()) }

  def kinase(p: Protein)(f: ProteinPattern => K): (Lst[K], KB[K], Unit) =
    db.query(Tables.Kinases)(pp => {
      if(pp.protein == p) Lst.singleton(f(pp))
      else Lst.empty[K]
    }) match { case (db, ks) => (ks, KB(db), ()) }

}

object KB {
  private object Tables {
    object Rules extends TableId[Rule]
    object PhosphoSites extends TableId[(Protein, Protein, Site)]
    object Kinases extends TableId[ProteinPattern]
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
      case AddPhosphoTarget(kin, sub, s) => kb.addPhosphoSite(kin, sub, s)
      case AddKinaseActivity(pp) => kb.addKinase(pp)
      case Rules(f) => kb.rules(f)
      case PhosphoTargets(f) => kb.phosphoTargets(f)
      case KinaseActivity(p, f) => kb.kinase(p)(f)
    })
  }
}