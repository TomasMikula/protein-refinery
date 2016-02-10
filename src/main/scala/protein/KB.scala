package protein

import protein.mechanism.{Site, Protein}

/**
  * Knowledge base.
  */
trait KB {

  def sitesOf(p: Protein): Seq[Site]

  def neighborsOf(p: Protein): Seq[capability.Binding]

  def phosphoSites(kinase: Protein, substrate: Protein): Seq[Site]

}
