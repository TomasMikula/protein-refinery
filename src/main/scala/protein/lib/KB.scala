package protein.lib

import nutcracker.IncSet._
import nutcracker.util.{ContF, FreeK, InjectK, Lst}
import nutcracker.util.ContF._
import nutcracker.{IncSet, PropagationLang}
import protein.db.{DB, DBLang, Table, TableId}

import scala.language.higherKinds

object KB {
  private object Tables {
    object Rules extends TableId[Rule]
    object PhosphoSites extends TableId[(Protein, Protein, Site)]
    object Kinases extends TableId[ProteinPattern]
  }

  def apply[K](
    rules: List[Rule] = Nil,
    phosphoSites: List[(Protein, Protein, Site)] = Nil
  ): DB[K] = {
    DB.empty[K]
      .setTable(Tables.Rules, Table[K, Rule](rules))
      .setTable(Tables.PhosphoSites, Table[K, (Protein, Protein, Site)](phosphoSites))
  }

  // instructions specialized to concrete tables
  def addRuleF[F[_[_], _]](r: Rule)(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    DBLang.insertF(Tables.Rules, r)
  def addPhosphoTargetF[F[_[_], _]](kinase: Protein, substrate: Protein, site: Site)(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    DBLang.insertF(Tables.PhosphoSites, (kinase, substrate, site))
  def addKinaseActivityF[F[_[_], _]](activeState: ProteinPattern)(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    DBLang.insertF(Tables.Kinases, activeState)
  def rulesF[F[_[_], _]](f: Rule => Lst[FreeK[F, Unit]])(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    DBLang.queryF(Tables.Rules)(f)
  def phosphoTargetsF[F[_[_], _]](f: (Protein, Protein, Site) => FreeK[F, Unit])(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    DBLang.queryF(Tables.PhosphoSites)({ case (k, s, ss) => Lst.singleton(f(k, s, ss)) })
  def kinaseActivityF[F[_[_], _]](p: Protein)(f: ProteinPattern => FreeK[F, Unit])(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    DBLang.queryF(Tables.Kinases)(pp => {
      if(pp.protein == p) Lst.singleton(f(pp))
      else Lst.empty
    })

  // KB queries in CPS style
  def rulesC[F[_[_], _]](implicit inj: InjectK[DBLang, F]): ContF[F, Rule] =
    ContF(f => rulesF[F](f andThen Lst.singleton))
  def phosphoTargetsC[F[_[_], _]](implicit inj: InjectK[DBLang, F]): ContF[F, (Protein, Protein, Site)] =
    ContF(f => phosphoTargetsF[F]((k, s, ss) => f((k, s, ss))))
  def kinaseActivityC[F[_[_], _]](kinase: Protein)(implicit inj: InjectK[DBLang, F]): ContF[F, ProteinPattern] =
    ContF(f => kinaseActivityF[F](kinase)(f))

  // derived queries

  def bindingsOfF[F[_[_], _]](p: Protein)(f: Binding => FreeK[F, Unit])(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    rulesF[F](rule => (rule.linksAgentTo(p), f) mapRev_::: Lst.empty)
  def bindingsOfC[F[_[_], _]](p: Protein)(implicit inj: InjectK[DBLang, F]): ContF[F, Binding] =
    ContF(f => bindingsOfF[F](p)(f))
  def bindingsOfS[F[_[_], _]](p: Protein)(implicit i: InjectK[DBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Binding]] =
    IncSet.collect(bindingsOfC(p))

  def phosphoSitesF[F[_[_], _]](kinase: Protein, substrate: Protein)(f: Site => FreeK[F, Unit])(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    phosphoTargetsF[F]((k, s, ss) => if(kinase == k && substrate == s) f(ss) else FreeK.pure(()))
  def phosphoSitesC[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit inj: InjectK[DBLang, F]): ContF[F, Site] =
    ContF(f => phosphoSitesF[F](kinase, substrate)(f))
  def phosphoSitesS[F[_[_], _]](kinase: Protein, substrate: Protein)(implicit i: InjectK[DBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Site]] =
    IncSet.collect(phosphoSitesC(kinase, substrate))

  def phosphoSitesS[F[_[_], _]](substrate: Protein)(implicit i: InjectK[DBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Site]] =
    for {
      res <- IncSet.init[F, Site]
      _   <- phosphoTargetsF[F]((k, s, ss) => if(s == substrate) IncSet.insert(ss, res) else FreeK.pure(()))
    } yield res
  def phosphoSitesC[F[_[_], _]](substrate: Protein)(implicit i: InjectK[DBLang, F], j: InjectK[PropagationLang, F]): ContF[F, Site] =
    phosphoSitesS[F](substrate).map(IncSet.forEach(_)).wrapEffect

  def kinasesOfF[F[_[_], _]](substrate: Protein, site: Site)(f: Protein => FreeK[F, Unit])(implicit inj: InjectK[DBLang, F]): FreeK[F, Unit] =
    phosphoTargetsF[F]((k, s, ss) => if(substrate == s && site == ss) f(k) else FreeK.pure(()))
  def kinasesOfC[F[_[_], _]](substrate: Protein, site: Site)(implicit inj: InjectK[DBLang, F]): ContF[F, Protein] =
    ContF(f => kinasesOfF[F](substrate, site)(f))
  def kinasesOfS[F[_[_], _]](substrate: Protein, site: Site)(implicit i: InjectK[DBLang, F], j: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[Protein]] =
    IncSet.collect(kinasesOfC(substrate, site))

}