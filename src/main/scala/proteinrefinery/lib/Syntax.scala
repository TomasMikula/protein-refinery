package proteinrefinery.lib

import nutcracker.util.EqualK
import proteinrefinery.lib.ProteinModifications.LocalSiteId
import proteinrefinery.lib.SiteState.SiteState

import scala.language.{higherKinds, implicitConversions}

trait Syntax[Ref[_]] {

  implicit class SymbolOps(sym: Symbol) {

    def apply(ss: (SiteLabel, SiteState)*): ProteinPattern[Ref] =
      ProteinPattern(Protein(sym), ProteinModifications.from(ss:_*))

    def phosphorylates(s: Protein): KinaseSubstratePair = Protein(sym).phosphorylates(s)

    def @@ (s: SiteLabel): BindingPartnerPattern[Ref] =
      BindingPartnerPattern(Protein(sym), LocalSiteId[Ref](s))

    def ~(s: SiteState): (SiteLabel, SiteState) = (SiteLabel(sym.name), s)

  }

  implicit class ProteinOps(p: Protein) {

    def apply(ss: (SiteLabel, SiteState)*): ProteinPattern[Ref] =
      ProteinPattern(p, ProteinModifications.from(ss:_*))

    def @@ (s: SiteLabel): BindingPartnerPattern[Ref] =
      BindingPartnerPattern(p, LocalSiteId[Ref](s))

    def phosphorylates(s: Protein): KinaseSubstratePair = KinaseSubstratePair(p, s)
  }

  implicit class ProteinPatternOps[Var[_]](p: ProteinPattern[Var]) {

    def @@ (s: SiteLabel): BindingPartnerPattern[Var] = BindingPartnerPattern(p, LocalSiteId[Var](s))

  }

  implicit class BindingPartnerPatternOps[Var[_]](bp: BindingPartnerPattern[Var]) {

    def binds(that: BindingPartnerPattern[Var])(implicit ev: EqualK[Var]): Binding[Var] = bp bind that

  }

  case class KinaseSubstratePair(kinase: Protein, substrate: Protein) {
    def at[Var[_]](targetSite: SiteLabel): PhosphoTriple[Var] = PhosphoTriple(kinase, substrate, ISite[Var](targetSite))
  }

  implicit def symbolToProtein(sym: Symbol): Protein = Protein(sym)

  implicit def symbolToSite(sym: Symbol): SiteLabel = SiteLabel(sym.name)

  implicit def stringToState(s: String): SiteState = SiteState(s)


  // examples
  private def examples(implicit ev: EqualK[Ref]) = {
    'A@@'x binds 'B@@'y
    'A('x~"p")
    'A('x~"p")@@'z
    'A('x~"p")@@'z binds 'B('y~"u")@@'w
  }
}

object Syntax {
  def apply[Ref[_]]: Syntax[Ref] = new Syntax[Ref]{}
}