package protein.capability

import protein.mechanism.{Site, Protein}

case class Phosphorylates(kinase: Protein, substrate: Protein, phosphoSite: Site)
